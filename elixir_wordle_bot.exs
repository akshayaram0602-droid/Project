
defmodule ElixirWordleBot do
  @moduledoc """
  An Elixir bot to play the Wordle game on wordle.we4shakthi.in.
  """

  @host "wordle.we4shakthi.in"
  @name "durga" # As used in the Haskell code

  def run do
    # Start the HTTP client
    HTTPoison.start()

    # 1. Register the player and get session cookie and ID
    case register_player() do
      {:ok, id, cookie} ->
        IO.puts "Player registered with ID: #{id}"
        # 2. Create a game
        case create_game(id, cookie) do
          {:ok, new_cookie} ->
            IO.puts "Game created successfully."
            # 3. Load words and start the game loop
            words = load_words("5words.txt")
            game_loop(id, new_cookie, words, 1)
          {:error, reason} ->
            IO.puts "Failed to create game: #{reason}"
        end
      {:error, reason} ->
        IO.puts "Failed to register player: #{reason}"
    end
  end

  defp register_player do
    url = "https://#{@host}/game/register"
    body = %{mode: "wordle", name: @name} |> Jason.encode!()
    headers = [{"Content-Type", "application/json"}]

    case HTTPoison.post(url, body, headers, [follow_redirect: true]) do
      {:ok, %{status_code: status_code, headers: response_headers, body: response_body}} when status_code in [200, 201] ->
        case Jason.decode(response_body) do
          {:ok, %{"id" => id}} ->
            set_cookie_header = Enum.find(response_headers, fn {key, _value} -> key == "Set-Cookie" end)
            cookie =
              case set_cookie_header do
                {"Set-Cookie", value} ->
                  String.split(value, ";") |> List.first()
                _ ->
                  nil
              end
            {:ok, id, cookie}
          _ ->
            {:error, "Failed to parse registration response"}
        end
      {:ok, %{status_code: status_code, body: body}} ->
        {:error, "Registration failed with status #{status_code}: #{body}"}
      {:error, %{reason: reason}} ->
        {:error, "HTTP request failed: #{reason}"}
    end
  end

  defp create_game(id, cookie) do
    url = "https://#{@host}/game/create"
    body = %{id: id, overwrite: true} |> Jason.encode!()
    headers = [{"Content-Type", "application/json"}, {"Cookie", cookie}]

    case HTTPoison.post(url, body, headers, [follow_redirect: true]) do
      {:ok, %{status_code: status_code, headers: response_headers}} when status_code in [200, 201] ->
        new_cookie =
          case Enum.find(response_headers, fn {key, _} -> key == "Set-Cookie" end) do
            {"Set-Cookie", value} -> String.split(value, ";") |> List.first()
            _ -> cookie # Fallback to old cookie if not found
          end
        {:ok, new_cookie}
      {:ok, %{status_code: status_code, body: body}} ->
        {:error, "Game creation failed with status #{status_code}: #{body}"}
      {:error, %{reason: reason}} ->
        {:error, "HTTP request failed: #{reason}"}
    end
  end

  defp game_loop(id, cookie, words, attempt_num) when attempt_num <= 6 do
    guess = Enum.random(words)
    IO.puts "\nAttempt #{attempt_num}: Is it '#{guess}'?"

    case make_guess(id, cookie, guess) do
      {:ok, feedback} ->
        IO.puts "Feedback: #{feedback}"
        if String.downcase(feedback) == "ggggg" do
          IO.puts "The computer guessed the correct word!"
        else
          new_words = filter_words(words, guess, feedback)
          game_loop(id, cookie, new_words, attempt_num + 1)
        end
      {:error, reason} ->
        IO.puts "Error making guess: #{reason}"
    end
  end

  defp game_loop(_, _, _, _) do
    IO.puts "The computer failed to guess the word."
  end

  defp make_guess(id, cookie, guess) do
    url = "https://#{@host}/game/guess"
    body = %{id: id, guess: guess} |> Jason.encode!()
    headers = [{"Content-Type", "application/json"}, {"Cookie", cookie}]

    case HTTPoison.post(url, body, headers, [follow_redirect: true]) do
      {:ok, %{status_code: 200, body: response_body}} ->
        case Jason.decode(response_body) do
          {:ok, %{"feedback" => feedback}} -> {:ok, feedback}
          _ -> {:error, "Failed to parse guess response"}
        end
      {:ok, %{status_code: status_code, body: body}} ->
        {:error, "Guess failed with status #{status_code}: #{body}"}
      {:error, %{reason: reason}} ->
        {:error, "HTTP request failed: #{reason}"}
    end
  end

  defp load_words(filename) do
    case File.read(filename) do
      {:ok, content} ->
        content
        |> String.split(~r/\s+/, trim: true)
        |> Enum.map(&String.upcase/1)
        |> Enum.filter(&(String.length(&1) == 5))
      {:error, reason} ->
        IO.puts "Error loading word list: #{reason}"
        []
    end
  end

  defp filter_words(words, guess, feedback) do
    guess_chars = String.graphemes(guess)
    feedback_chars = String.graphemes(String.downcase(feedback))

    {greens, yellows, reds} =
      Enum.zip(guess_chars, feedback_chars)
      |> Enum.with_index()
      |> Enum.reduce({%{}, %{}, MapSet.new()}, fn {{g_char, f_char}, index}, {greens, yellows, reds} ->
        case f_char do
          "g" -> {Map.put(greens, index, g_char), yellows, reds}
          "y" -> {greens, Map.update(yellows, g_char, [index], fn existing -> [index | existing] end), reds}
          "r" -> {greens, yellows, MapSet.put(reds, g_char)}
          _   -> {greens, yellows, reds}
        end
      end)

    Enum.filter(words, fn word ->
      word_chars = String.graphemes(word)

      # Green check
      greens_ok = Enum.all?(greens, fn {index, char} ->
        Enum.at(word_chars, index) == char
      end)

      # Yellow check
      yellows_ok = Enum.all?(yellows, fn {char, bad_indices} ->
        (char in word_chars) and Enum.all?(bad_indices, fn index -> Enum.at(word_chars, index) != char end)
      end)

      # Red check (with improvement for duplicate letters)
      reds_ok = Enum.all?(reds, fn char ->
        # A red letter can't be in the word, unless it's also a green or yellow letter.
        if Map.values(greens) |> Enum.member?(char) or Map.has_key?(yellows, char) do
          # This handles cases where a letter is both red and green/yellow in the guess
          # (e.g., guess "APPLE", secret "PAPER").
          # The number of occurrences of the char in the word must be equal to the number of
          # non-red occurrences in the guess.
          non_red_count = Enum.zip(guess_chars, feedback_chars) |> Enum.count(fn {c, f} -> c == char and f != "r" end)
          word_char_count = Enum.count(word_chars, fn c -> c == char end)
          word_char_count == non_red_count
        else
          char not in word_chars
        end
      end)

      greens_ok and yellows_ok and reds_ok
    end)
  end
end

ElixirWordleBot.run()
