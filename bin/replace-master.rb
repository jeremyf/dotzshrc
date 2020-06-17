#!/usr/bin/env ruby -wU
# frozen_string_literal: true

# A script to rename a single repository's branch FROM the given
# branch name TO the given branch name.
#
# This script assumes you're running the command within a git
# repository.  As I often use Github, this script will also rename the
# remote branch and remediate the default_branch.
#
# You will need to provide the GITHUB_OWNER and OAUTH_TOKEN for
# interacting with Github.
#
# This script handles how I've setup git, but may be useful as either
# a point of reference or to run yourself.
#
# You'll need to get your own OAUTH_TOKEN (see https://developer.github.com/apps/building-oauth-apps/authorizing-oauth-apps/)
#
# Example:
#
#   GITHUB_OWNER=jeremyf OAUTH_TOKEN=40-char-token ruby /path/to/replace-master.rb
#
FROM_BRANCH = ENV.fetch('FROM', 'master')
TO_BRANCH = ENV.fetch('TO', 'trunk')
GITHUB_OWNER = ENV.fetch('GITHUB_OWNER')
OAUTH_TOKEN = ENV.fetch('OAUTH_TOKEN')

pwd = Dir.pwd

def shell_command(cmd)
  return if system(cmd)

  exit!(2)
end

check_master = `git branch -a | grep "#{FROM_BRANCH}$" | awk '{ print $2}'`.strip
if check_master.empty?
  $stdout.puts "No '#{FROM_BRANCH}' branch found at '#{pwd}'"
  exit(0)
end

$stdout.puts 'Working on local clone'
unless `git status --porcelain`.strip.empty?
  warn "ABORT: '#{pwd}' has uncommitted changes"
  exit!(3)
end

shell_command("git checkout #{FROM_BRANCH}")
shell_command("git branch -m #{TO_BRANCH}")

$stdout.puts 'Working on possible remote'
remote_name = `git remote -v | grep "#{GITHUB_OWNER}/" | grep "(push)" | awk '{print $1}'`.strip
repository_url = `git remote -v | grep "#{GITHUB_OWNER}/" | grep "(push)" | awk '{print $2}'`
owner, repo = Array(repository_url.sub(/^.*github\.com./, '').split('/')[-2..-1]).map(&:strip)

if !repo || !owner
  $stdout.puts "No known remote for '#{pwd}' for username #{GITHUB_OWNER}. Done"
  exit!(0)
end

shell_command("git push -u #{remote_name} --no-verify")

repo = repo.sub(/\.git$/, '')
shell_command(%(curl -X PATCH -d '{"default_branch":"#{TO_BRANCH}"}' -H "Authorization: token #{OAUTH_TOKEN}" https://api.github.com/repos/#{owner}/#{repo}))

shell_command("git push #{remote_name} :#{FROM_BRANCH} --no-verify")
