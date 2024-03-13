import os

import autogen
from autogen import AssistantAgent
from autogen.agentchat.contrib.retrieve_user_proxy_agent import RetrieveUserProxyAgent
from dotenv import load_dotenv

load_dotenv()

# chromadb_client = Client()
# collection = chromadb_client.create_collection("testautogen")


# def read_file_paths(directory_path="."):
#     with open(".gitignore", "r") as f:
#         gitignore_patterns = f.read().splitlines()
#     spec = pathspec.PathSpec.from_lines(GitWildMatchPattern, gitignore_patterns)
#     all_files = [file for file in iter_tree_files(os.path.abspath(directory_path))]
#     filtered_files = [file for file in all_files if not spec.match_file(file)]
#     return filtered_files

# def read_file_contents(file_path):
#     with open(file_path, "r") as file:
#         return file.read()

# workspace_files = read_file_paths()

# collection.add(
#     documents=[read_file_contents(file_path) for file_path in workspace_files],
#     metadatas=[{"file_path": file_path} for file_path in workspace_files],
#     ids=[file_path for file_path in workspace_files],
# )

# results = collection.query(
#     query_texts=["All python files"],
#     n_results=100,
#     # where={"metadata_field": "is_equal_to_this"}, # optional filter
#     where_document={"$contains": "def"},  # optional filter
# )

# print(results)

# vector_db = create_vector_db_from_dir(
#     # client=chromadb_client,
#     dir_path="./testautogen",
#     collection_name="testautogen",
#     get_or_create=True,
#     extra_docs=True,
# )
# query_result = query_vector_db(
#     client=vector_db,
#     query_texts=["load_dotenv"],
#     collection_name="testautogen",
# )
# print(query_result)

directory_path = os.path.abspath("testautogen/.tmp/defillama")

config_list = [
    {
        "model": "mistral",
        "base_url": "http://localhost:11434/v1",
        "api_key": "ollama",
    }
]

llm_config = {
    # "temperature": 0,
    "cache_seed": 42,
    "config_list": config_list,
    "seed": 42,
    # "timeout": 60,
    "functions": [
        {
            "name": "retrieve_content",
            "description": "retrieve content for code generation and question answering.",
            "parameters": {
                "type": "object",
                "properties": {
                    "message": {
                        "type": "string",
                        "description": """Refined message which keeps the
                        original meaning and can be used to retrieve content for
                        code generation and question answering.""",
                    }
                },
                "required": ["message"],
            },
        },
    ],
}

code_execution_config = {
    "work_dir": directory_path,
    "use_docker": False,
}


def termination_msg(x):
    return x.get("content", "").rstrip().endswith("TERMINATE")


# assistant = RetrieveAssistantAgent(
#     name="assistant",
#     system_message="You are a helpful assistant.",
#     llm_config=llm_config,
# )
# ragproxyagent = RetrieveUserProxyAgent(
#     name="ragproxyagent",
#     retrieve_config={
#         "task": "qa",
#         "docs_path": "https://raw.githubusercontent.com/microsoft/autogen/main/README.md",
#     },
#     code_execution_config={
#         "work_dir": directory_path,
#         "use_docker": False,
#     },
# )
# assistant.reset()
# ragproxyagent.initiate_chat(assistant, problem="What is autogen?")

# assistant_agent = AssistantAgent(
#     "assistant",
#     llm_config=llm_config,
# )
# coder = AssistantAgent(
#     "coder",
#     llm_config=llm_config,
# )
# investor = AssistantAgent(
#     "investor",
#     llm_config=llm_config,
# )
# def is_termination_msg(x):
#     return x.get("content", "").rstrip().endswith("TERMINATE")
# userproxy = UserProxyAgent(
#     name="userproxy",
#     human_input_mode="NEVER",
#     # human_input_mode="TERMINATE",
#     max_consecutive_auto_reply=10,
#     # is_termination_msg=is_termination_msg,
#     code_execution_config={
#         "work_dir": directory_path,
#         "use_docker": False,
#     },
# )
# groupchat = GroupChat(agents=[userproxy, coder, investor], messages=[], max_round=12)
# manager = GroupChatManager(groupchat=groupchat, llm_config=llm_config)
# userproxy.initiate_chat(
#     manager,
#     message=f"""
# - Use the {directory_path} directory to store files and execute code from.
# - Create a python virtualenv in "{directory_path}/.venv".
#   - Use the command `python -m venv .venv` in a bash script.
#   - Execute the script to create the virtualenv.
# """,
# )
# userproxy.initiate_chat(
#     manager,
#     message=f"""
# - Before executing any code, make sure the python virtualenv is activated.
# - Crawl the defilama.com website.
#   - Navigate to the base chain page and give me an overview of the projects that
#     have been consistently growing in the last 6 months.
# - Install the required packages for the generated code.
# """,
# )

boss = autogen.UserProxyAgent(
    name="Boss",
    is_termination_msg=termination_msg,
    human_input_mode="TERMINATE",
    system_message="The boss who ask questions and gives tasks.",
    code_execution_config=code_execution_config,
)

boss_aid = RetrieveUserProxyAgent(
    name="Boss_Assistant",
    is_termination_msg=termination_msg,
    system_message="Assistant who has extra content retrieval power for solving difficult problems.",
    human_input_mode="NEVER",
    max_consecutive_auto_reply=3,
    retrieve_config={
        "task": "qa",
    },
    code_execution_config=False,  # we don't want to execute code in this case.
)

coder = AssistantAgent(
    name="Senior_Python_Engineer",
    is_termination_msg=termination_msg,
    system_message="You are a senior python engineer. Reply `TERMINATE` in the end when everything is done.",
    llm_config=llm_config,
)

pm = autogen.AssistantAgent(
    name="Product_Manager",
    is_termination_msg=termination_msg,
    system_message="You are a product manager. Reply `TERMINATE` in the end when everything is done.",
    llm_config=llm_config,
)

reviewer = autogen.AssistantAgent(
    name="Code_Reviewer",
    is_termination_msg=termination_msg,
    system_message="You are a code reviewer. Reply `TERMINATE` in the end when everything is done.",
    llm_config=llm_config,
)

agents = [boss, coder, pm, reviewer]


def retrieve_content(message, n_results=3):
    boss_aid.n_results = n_results  # Set the number of results to be retrieved.
    # Check if we need to update the context.
    update_context_case1, update_context_case2 = boss_aid._check_update_context(message)
    if (update_context_case1 or update_context_case2) and boss_aid.update_context:
        boss_aid.problem = (
            message if not hasattr(boss_aid, "problem") else boss_aid.problem
        )
        _, ret_msg = boss_aid._generate_retrieve_user_reply(message)
    else:
        ret_msg = boss_aid.generate_init_message(message, n_results=n_results)
    return ret_msg if ret_msg else message


for agent in agents:
    # register functions for all agents.
    agent.register_function(
        function_map={
            "retrieve_content": retrieve_content,
        }
    )

groupchat = autogen.GroupChat(agents=agents, messages=[], max_round=12)

manager = autogen.GroupChatManager(
    groupchat=groupchat,
    llm_config={
        "cache_seed": 42,
        "config_list": config_list,
        "seed": 42,
        # "timeout": 60,
    },
)

# Start chatting with the boss as this is the user proxy agent.
boss.initiate_chat(
    manager,
    message="How to use spark for parallel training in FLAML? Give me sample code.",
)

print(123)
