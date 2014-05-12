
defmodule Eroop do

  defmacro __using__(_opts) do
    quote do
      import Eroop
    end
  end

  defmacro actor(name, do: block) do
    def_supervisor name
    def_module name, block

  end

  defmacro init(block) do
  end

  defmacro sync(header, do: block) do
    def_method(header, block)
  end

  defmacro async(name, do: block) do
  end

  defmacro terminate(name, do: block) do
  end

  defp def_supervisor(name) do
  end


  defp def_module(name, block) do
    quote do

      defmodule unquote(name) do
        use GenServer.Behaviour

        # class APIs
        def new() do
          IO.puts "new"
          {:ok, pid} = :gen_server.start_link(__MODULE__, [], [])
          {__MODULE__, pid}
        end

        def start_link() do
        end

        # instance APIs
        unquote block

        # callbacks of gen_server
        def init(state) do
          {:ok, state}
        end

        def handle_call({fun, args}, _from, state) do
          {:reply, :apply, state}
        end

        def handle_cast({fun, args}, state) do
          {:noreply, state}
        end

      end

    end
  end

  defp def_method(header, block) do
    def_priv_method(header, block)
    def_pub_method(header, block)
  end

  defp def_priv_method(header, block) do
    quote do
      def unquote(header), do: unquote block
    end
  end

  defp def_pub_method(header, block) do
    {name, line, args1} = header
    args2 = case args1 do
      nil -> []
      _ -> args1
    end
    args3 = args2 ++ [{:inner_pid, [], nil}]
    header1 = {name, line, args3}

    quote do
      def unquote(header1) do
        {_module, pid} = inner_pid
        send_msg({name, args1}, pid, :sync)
      end
    end
  end

  defp send_msg(msg, pid, :sync) do
    :gen_server.call(pid, msg)
  end

  defp send_msg(msg, pid, :async) do
    :gen_server.cast(pid, msg)
  end

  # transform the block in function
  defp transform_block(block) do
  end

end
