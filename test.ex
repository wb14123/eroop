
use Eroop

actor Test do

  sync hello(msg, type) do
    IO.puts "hello, world"
  end

end

t = Test.new
t.hello(1)
