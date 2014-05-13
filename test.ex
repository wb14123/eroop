
use Eroop

actor Test do

  init do
  end

  sync add(num) do
    @counter = num
  end

  sync get() do
    IO.puts @couter
  end

  sync hello(msg, type) do
    @a = 1
    :timer.sleep(1000)
    :io.format("hello, msg: ~p, type: ~p~n", [msg, type])
  end

  sync hi do
    :timer.sleep(1000)
    :io.format("hi~n")
  end

end

t = Test.new
# t.hi
IO.puts "I just said hi"
t.hello(1, :info)
IO.puts "I just said hello"

t.add(5)
t.get
