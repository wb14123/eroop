
defmodule Eroop do

  defmacro __using__(_opts) do
    quote do
      import Kernel, except: [@: 1]
      import Eroop
    end
  end

  defmacro init(do: block) do
    quote do
      defp __init do
        var!(state) = %{}
        unquote transform block
        var!(state)
      end
    end
  end

  defmacro sync(header, do: block), do: def_method(header, block, :call)
  defmacro async(header, do: block), do: def_method(header, block, :cast)
  defmacro terminate(do: block), do: :ok

  defmacro @({attr, _, _}) do
    quote do: var!(state)[unquote(attr)]
  end

  defmacro actor(name, do: block) do
    quote do

      defmodule unquote(name) do
        use GenServer.Behaviour

        # instance APIs
        unquote block

        def get_state(timeout, {__MODULE__, pid}), do: :sys.get_status(pid, timeout)

        # class APIs
        def new() do
          {:ok, pid} = :gen_server.start_link(__MODULE__, [], [])
          {__MODULE__, pid}
        end

        def start_link() do
        end

        # callbacks of gen_server
        def init(_) do
          state = __init
          {:ok, state}
        end

        def handle_call({fun, args}, _from, state) do
          {new_state, reply} = :erlang.apply(__MODULE__, fun, [state | args])
          {:reply, reply, new_state}
        end

        def handle_cast({fun, args}, state) do
          {new_state, reply} = :erlang.apply(__MODULE__, fun, [state | args])
          {:noreply, new_state}
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

  defp priv_name(name), do: list_to_atom('__' ++ atom_to_list(name))

  defp def_priv_method({name, line, nil}, block), do: def_priv_method({name, line, []}, block)
  defp def_priv_method({name, _, params}, block) do
    quote do
      def unquote(priv_name name)(var!(state), unquote_splicing(params)) do
        reply = (unquote transform block)
        {var!(state), reply}
      end
    end
  end

  defp def_pub_method({:when, _, [{name, _, params} | guards]}, type) do
    quote do
      def unquote(name)(unquote_splicing(params), {_, pid})
        when unquote_splicing(guards) do
          :erlang.apply(:gen_server, unquote(type), [pid, {unquote(priv_name name), unquote(params)}])
        end
    end
  end

  defp def_pub_method({name, line, nil}, type), do: def_pub_method({name, line, []}, type)
  defp def_pub_method({name, _, params}, type) do
    quote do
      def unquote(name)(unquote_splicing(params), {_, pid}) do
        :erlang.apply(:gen_server, unquote(type), [pid, {unquote(priv_name name), unquote(params)}])
      end
    end
  end

  defp transform({:=, _, [{:@, _, [{v1, _, _}]}, v2]}) do
    quote do: var!(state) = Dict.put(var!(state), unquote(v1), unquote(transform v2))
  end
  defp transform({sym, line, r = [_|_]}) do
    {sym, line, Enum.map(r, fn x -> transform x end)}
  end
  defp transform(x), do: x

end
