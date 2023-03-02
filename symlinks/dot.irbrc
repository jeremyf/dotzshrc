# frozen_string_literal: true

# In ruby 2.7.0 and irb 1.3.x, the navigate history feature does not work
# for it to work, launch `irb --legacy`
require 'irb/completion'
require 'irb/ext/save-history'
# IRB.conf[:PROMPT_MODE] = :SIMPLE

require 'fancy_irb'

DEFAULT_OPTIONS =
  FancyIrb.start(
    {
      :rocket_mode     => true,       # activate or deactivate #=> rocket
      :rocket_prompt   => '#=> ',     # prompt to use for the rocket
      :result_prompt   => '=> ',      # prompt to use for normal output
      :unicode_display_width => true, # set to false if you don't want to check for proper
      # string width for better performance
      :colorize => {                  # colors hash. Set to nil to deactivate colors
                    :rocket_prompt => [:white],
                    :result_prompt => [:white],
                    :input_prompt  => nil,
                    :irb_errors    => [:red, :clean],
                    :stderr        => [:red, :bright],
                    :stdout        => nil,
                    :input         => nil,
                   },
    }
  )