
use Eroop

actor Test do

  init do
    @counter = 1
  end

  async add(num) do
    :timer.sleep(1000)
    IO.puts "I will really add now"
    @counter = @counter + num
  end

  sync get() do
    @counter
  end

end

t = Test.new
t.add(5)
IO.puts "I just add 5"
IO.puts t.get
