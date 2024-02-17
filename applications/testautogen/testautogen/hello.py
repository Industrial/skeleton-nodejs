import os

from autogen import AssistantAgent, UserProxyAgent
from dotenv import load_dotenv

load_dotenv()


def hello():
    """Return a friendly greeting."""
    return "Hello testautogen"


# config_list = config_list_from_json(env_or_file="OAI_CONFIG_LIST")
config_list = {
    "config_list": [
        {
            "model": "gpt-3.5-turbo",
            "api_key": os.getenv("OPENAI_API_KEY"),
        }
    ]
}

assistant = AssistantAgent("assistant", llm_config={"config_list": config_list})

user_proxy = UserProxyAgent(
    "user_proxy", code_execution_config={"work_dir": "coding", "use_docker": False}
)

user_proxy.initiate_chat(
    assistant, message="Plot a chart of NVDA and TESLA stock price change YTD."
)
