#! /bin/sh

rm *.beam
elixirc eroop.ex
elixir test.ex
