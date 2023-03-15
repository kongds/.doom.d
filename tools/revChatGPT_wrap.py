import sys
import argparse
from revChatGPT import V1, V3


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
    parser.add_argument(
        "--version",
        type=int,
        default=3,
        help="Version of chatbot to use",
    )

    args = parser.parse_args()
    # Initialize chatbot
    if args.version == 1:
        from revChatGPT.V1 import configure
        config = configure()
        chatbot = V1.Chatbot(config,
                             conversation_id=config.get("conversation_id"),
                             parent_id=config.get("parent_id"),
                             collect_data=False,)
    else:
        chatbot = V3.Chatbot(api_key=args.api_key, system_prompt=args.base_prompt)
    print('Logging in...')
    while True:
        print()
        # Display the prompt
        print("\nYou:\n", end="")
        # Initialize an empty list to store the input lines
        lines = []

        # Read lines of input until the user enters an empty line
        count = 0
        while True:
            line = input()
            if line == "" and (len(lines) > 1 and lines[-1] == ""):
                count += 1
                if count == 3:
                    break
            else:
                count = 0
            lines.append(line)

        print("Chatbot: ", flush=True)
        # Join the lines, separated by newlines, and store the result
        user_input = "\n".join(lines)
        if args.version == 1:
            prev_text = ""
            for data in chatbot.ask(user_input):
                message = data["message"][len(prev_text) :]
                print(message, end="", flush=True)
                prev_text = data["message"]
        else:
            for response in chatbot.ask_stream(user_input, temperature=args.temperature):
                print(response, end="", flush=True)
        print()

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\nExiting...")
        sys.exit()
