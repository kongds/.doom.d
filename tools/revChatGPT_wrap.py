import sys
import argparse
from revChatGPT import V3


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--api_key",
        type=str,
        required=True,
        help="OpenAI API key",
    )
    parser.add_argument(
        "--temperature",
        type=float,
        default=0.5,
        help="Temperature for response",
    )
    parser.add_argument(
        "--base_prompt",
        type=str,
        default="You are ChatGPT, a large language model trained by OpenAI. Respond conversationally",
        help="Base prompt for chatbot",
    )
    args = parser.parse_args()
    # Initialize chatbot
    chatbot = V3.Chatbot(api_key=args.api_key, system_prompt=args.base_prompt)
    print('Logging in...')
    while True:
        print()
        # Display the prompt
        print("\nYou:\n", end="")
        # Initialize an empty list to store the input lines
        lines = []

        # Read lines of input until the user enters an empty line
        while True:
            line = input()
            if line == "":
                break
            lines.append(line)

        print("Chatbot: ", flush=True)
        # Join the lines, separated by newlines, and store the result
        user_input = "\n".join(lines)
        for response in chatbot.ask_stream(user_input, temperature=args.temperature):
            print(response, end="", flush=True)
        print()

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\nExiting...")
        sys.exit()
