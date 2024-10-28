import sys
import argparse
import tiktoken
from revChatGPT import V1, V3


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--api_key",
        type=str,
        required=False,
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
    parser.add_argument(
        "--model",
        type=str,
        default="gpt-3.5-turbo",
        help="engine to use",
    )

    args = parser.parse_args()
    if args.model == 'gpt-4':
        args.model = 'gpt-4-turbo'
        ## use chat
        # args.version = 1
        pass
    # Initialize chatbot
    if args.version == 1:
        from revChatGPT.V1 import configure
        config = configure()
        chatbot = V1.Chatbot(config)
    else:
        chatbot = V3.Chatbot(api_key=args.api_key,
                             system_prompt=args.base_prompt, engine=args.model,
                             max_tokens=4096)
    print('Logging in...')
    while True:
        print()
        # Display the prompt
        if  args.version != 1 and len(chatbot.conversation["default"]) > 1:
            tiktoken.model.MODEL_TO_ENCODING["gpt-4"] = "cl100k_base"
            encoding = tiktoken.encoding_for_model(chatbot.engine)
            prompt_tokens, reply_tokens = 0, 0
            for message in chatbot.conversation["default"]:
                # every message follows <im_start>{role/name}\n{content}<im_end>\n
                prompt_tokens += 5
                for key, value in message.items():
                    if message['role'] == 'assistant':
                        reply_tokens += len(encoding.encode(value))
                    else:
                        prompt_tokens += len(encoding.encode(value))
                    if key == "name":  # if there's a name, the role is omitted
                        prompt_tokens += 5  # role is always required and always 1 token
            prompt_tokens += 5  # every reply is primed with <im_start>assistant
            if 'gpt-4' in chatbot.engine:
                price = prompt_tokens / 1000  * 0.03 + reply_tokens / 1000 * 0.06
            else:
                price = (prompt_tokens + reply_tokens) / 1000 * 0.002
            print(f"\n{prompt_tokens} {reply_tokens} {'{:.5f}'.format(price)} You:\n", end="")
        else:
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
