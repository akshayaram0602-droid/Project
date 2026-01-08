import requests
import random

class WordleSolver:
    """A class to manage the logic of solving a Wordle game."""
    instructions = """For every guessed word, the server provides feedback.
         g = Green (correct letter, correct position)
         y = Yellow (correct letter, wrong position)
         r = Red (letter not in the word)"""

    def __init__(self, session, player_id, all_words):
        self.session = session
        self.player_id = player_id
        self.available_words = all_words[:]
        self.chances = 6
        self.attempt_num = 0
        self.status = "PLAY"
        self.guess = ""

    def _start_game(self):
        """Creates a new game on the server."""
        response = self.session.post(
            "https://wordle.we4shakthi.in/game/create", 
            json={"id": self.player_id, "overwrite": True}
        )
        if response.status_code == 200:
            print("Game already exists.")
        elif response.status_code == 201:
            print("Game has been created.")
        else:
            print(f"Game creation failed: HTTP {response.status_code}")

    def _filter_words(self, feedback: str):
        """Filters the word list based on the server's feedback."""
        def drop_blacks(blacks: str, word: str) -> bool: 
            return all(b not in word for b in blacks)

        def pick_greens(greens: list, word: str) -> bool:
            for i in range(5):
                if greens[i] != " " and word[i] != greens[i]:
                    return False
            return True

        def pick_ambers(ambers: dict, word: str) -> bool:
            for ch, bad_pos in ambers.items():
                if ch not in word:
                    return False
                if any(word[pos] == ch for pos in bad_pos):
                    return False  
            return True

        greens = [" ", " ", " ", " ", " "]
        blacks = ""
        ambers = {}

        for i in range(5):
            letter = feedback[i].lower()
            guess_char = self.guess[i]
            if letter == "g":
                greens[i] = guess_char
            elif letter == "y":
                if guess_char not in ambers:
                    ambers[guess_char] = []
                ambers[guess_char].append(i)
            elif letter == "r":
                blacks += guess_char

        self.available_words = [
            word for word in self.available_words
            if drop_blacks(blacks, word)
            and pick_greens(greens, word)
            and pick_ambers(ambers, word)
        ]

    def play(self): 
        """Runs the main game loop."""
        self._start_game()
        print(self.instructions)
        
        while self.attempt_num < self.chances and self.status == "PLAY":
            self.attempt_num += 1
            if not self.available_words:
                print("No more possible words to guess.")
                break

            random.shuffle(self.available_words)
            self.guess = self.available_words.pop(0)
            print(f"\nAttempt {self.attempt_num}: Is it '{self.guess}'?")
            
            response = self.session.post(
                "https://wordle.we4shakthi.in/game/guess", 
                json={"guess": self.guess, "id": self.player_id}
            )
            
            try:
                result = response.json()
            except requests.exceptions.JSONDecodeError:
                print(f"Error: Could not decode JSON from server. Response: {response.text}")
                continue

            feedback = result.get('feedback')
            message = result.get('message', 'No message')
            print(f"Message: {message}")

            if not feedback or len(feedback) != 5:
                print("Invalid feedback received, trying another word.")
                continue

            print(f"Feedback: {feedback}")
            if all(c.lower() == "g" for c in feedback):
                print("The computer guessed the correct word!")
                self.status = "WON"
                return

            self._filter_words(feedback.lower())

        if self.status != "WON":
            print("The computer failed to guess the word.")

def main():
    """Main function to set up and run the Wordle bot."""
    player_name = "durga"
    
    try:
        with open("5words.txt", "r") as file:
            all_words = [line.strip() for line in file if len(line.strip()) == 5]
    except FileNotFoundError:
        print("Error: 5words.txt not found. Please create it with a list of five-letter words.")
        return

    # Create a single session to be reused for all requests
    with requests.Session() as session:
        # Register the player
        try:
            response = session.post(
                "https://wordle.we4shakthi.in/game/register", 
                json={"mode": "wordle", "name": player_name}
            )
            response.raise_for_status() # Raise an exception for bad status codes
            player_id = response.json().get("id")
            if not player_id:
                print("Could not get player ID from server.")
                return
        except requests.exceptions.RequestException as e:
            print(f"Error registering player: {e}")
            return

        print(f"Registered player '{player_name}' with ID: {player_id}")

        # Create and play the game
        game = WordleSolver(session, player_id, all_words)
        game.play()

if __name__ == "__main__":
    main()