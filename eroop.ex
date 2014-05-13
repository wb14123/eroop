
defmodule Eroop do

  defmacro __using__(_opts) do
    quote do: import Eroop
  end

  defmacro actor(name, do: block) do
    def_supervisor name
    def_module name, block
  end

  defmacro init(do: block), do: :ok
  defmacro sync(header, do: block), do: def_method(header, block, :call)
  defmacro async(header, do: block), do: def_method(header, block, :cast)
  defmacro terminate(do: block), do: :ok

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
          self = {__MODULE__, pid}
          self
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
          {:reply, :erlang.apply(__MODULE__, fun, args), state}
        end

        def handle_cast({fun, args}, state) do
          :erlang.apply(__MODULE__, fun, args)
          {:noreply, state}
        end

      end

    end
  end

  defp def_method(header, block, type) do
    {:__block__, [],
      [
        def_priv_method(header, block),
        def_pub_method(header, type)
      ]}
  end

  defp def_priv_method(header, block) do
    quote do
      def unquote(header), do: unquote block
    end
  end

  defp def_pub_method({:when, _, [{name, _, params} | guards]}, type) do
    quote do
      def unquote(name)(unquote_splicing(params), {_, pid})
        when unquote_splicing(guards) do
          :erlang.apply(:gen_server, unquote(type), [pid, {unquote(name), unquote(params)}])
        end
    end
  end

  defp def_pub_method({name, line, nil}, type), do: def_pub_method({name, line, []}, type)
  defp def_pub_method({name, _, params}, type) do
    quote do
      def unquote(name)(unquote_splicing(params), {_, pid}) do
        :erlang.apply(:gen_server, unquote(type), [pid, {unquote(name), unquote(params)}])
      end
    end
  end

end
