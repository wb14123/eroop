#! /bin/sh

rm Elixir.Eroop.beam
elixirc eroop.ex
elixir test.ex
