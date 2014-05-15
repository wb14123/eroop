
defmodule EroopTest do
  use ExUnit.Case

  test "basic" do
    defmodule Basic do
      use Eroop

      init _(init_count, time_wait) do
        @counter = init_count
        @time = time_wait
      end

      async add(num) do
        :timer.sleep(@time)
        @counter = @counter + num
      end

      sync get do
        @counter
      end
    end

    Basic.start_sup []

    time_wait = 100
    c = Basic.new 2, time_wait
    {s1, s2, s3} = :erlang.now
    c.add(5)
    {e1, e2, e3} = :erlang.now
    time = (e1 - s1) * 1000000 * 1000000 + (e2 - s2) * 100000 + (e3 - s3)
    count = c.get
    assert time < time_wait * 1000
    assert count == 7
  end

end
