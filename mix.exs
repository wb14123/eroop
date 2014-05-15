defmodule Eroop.Mixfile do
  use Mix.Project

  def project do
    [
      project: "Eroop",
      version: "0.0.1",
      elixir: "~> 0.13.0",
      app: :eroop,
      package: [
        contributors: ["Bin Wang"],
        licenses: ["GPL"]
      ],
      description: "Make it simple to build OTP application"
    ]
  end

  def application, do: []

  def deps, do: []
end
