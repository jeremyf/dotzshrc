# frozen_string_literal: true

# In ruby 2.7.0 and irb 1.3.x, the navigate history feature does not work
# for it to work, launch `irb --legacy`
require 'irb/completion'
require 'irb/ext/save-history'
