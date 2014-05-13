
use Eroop

actor Test do

  sync hello(msg, type) do
    :io.format("hello, msg: ~p, type: ~p~n", [msg, type])
  end

  async hi do
    :timer.sleep(1000)
    :io.format("hi~n")
  end

end

t = Test.new
t.hi
IO.puts "I just say hi"
t.hello(1, :info)
IO.puts "I just say hello"
