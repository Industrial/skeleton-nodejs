import os

from autogen import AssistantAgent, GroupChat, GroupChatManager, UserProxyAgent
from dotenv import load_dotenv

load_dotenv()

config_list = [
    {
        "model": "mistral",
        "base_url": "http://localhost:11434/v1",
        "api_key": "ollama",
    }
]

llm_config = {
    "cache_seed": 41,
    "config_list": config_list,
    "temperature": 0,
}

assistant_agent = AssistantAgent(
    "assistant",
    llm_config=llm_config,
)

coder = AssistantAgent(
    "coder",
    llm_config=llm_config,
)

investor = AssistantAgent(
    "investor",
    llm_config=llm_config,
)


def is_termination_msg(x):
    return x.get("content", "").rstrip().endswith("TERMINATE")


directory_path = os.path.abspath("testautogen/.tmp/defillama")

userproxy = UserProxyAgent(
    name="userproxy",
    human_input_mode="NEVER",
    # human_input_mode="TERMINATE",
    max_consecutive_auto_reply=10,
    # is_termination_msg=is_termination_msg,
    code_execution_config={
        "work_dir": directory_path,
        "use_docker": False,
    },
)

groupchat = GroupChat(agents=[userproxy, coder, investor], messages=[], max_round=12)
manager = GroupChatManager(groupchat=groupchat, llm_config=llm_config)

userproxy.initiate_chat(
    manager,
    message=f"""
- Use the {directory_path} directory to store files and execute code from.
- Create a python virtualenv in "{directory_path}/.venv".
  - Use the command `python -m venv .venv` in a bash script.
  - Execute the script to create the virtualenv.
""",
)

userproxy.initiate_chat(
    manager,
    message=f"""
- Before executing any code, make sure the python virtualenv is activated.
- Crawl the defilama.com website.
  - Navigate to the base chain page and give me an overview of the projects that
    have been consistently growing in the last 6 months.
- Install the required packages for the generated code.
""",
)

print(123)
