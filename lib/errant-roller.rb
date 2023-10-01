# The number of scenarios we run to determine the average.  In my
# isolated testing around 4000 scenarios is where I find stability in
# the results.
SCENARIOS = 10_000


# The values of the hash are position:
# first is the "number of attributes" and second is the roller function
ROLLERS = {
  "4d4" => [
    4,
    -> { 4 + (1..4).sum { rand(4) } }],
  "5d4 drop lowest" => [
    4,
    # Sort is in ascending order, so we "drop" the first element
    -> { 4 + (1..5).map { rand(4) }.sort[1..-1].sum }],
  "3d6" => [
    6,
    -> { 3 + (1..3).sum { rand(6) } }],
  "4d6 drop lowest" => [
    6,
    # Sort is in ascending order, so we "drop" the first element
    -> { 3 + (1..4).map { rand(6) }.sort[1..-1].sum } ]
}

ROLLERS.each do |strategy, (number_of_attributes, roller)|
  results = Array.new(number_of_attributes) { 0 }
  (1..SCENARIOS).each do
    result = (1..number_of_attributes).map { roller.call }.sort

    (0...number_of_attributes).each do |i|
      results[i] += result[i]
    end
  end

  print "\n- Strategy: #{strategy} :: "
  results.reverse.each do |result|
    print "#{(result.to_f / SCENARIOS).round} "
  end
end
puts
