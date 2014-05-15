
defmodule Test do
  use Eroop

  init _(init_count, _whatever) do
    @counter = init_count
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

t = Test.new 2, 3
t.add(5)
IO.puts "I just add 5"
IO.puts t.get
