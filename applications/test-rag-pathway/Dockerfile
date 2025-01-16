FROM pathwaycom/pathway:latest

WORKDIR /app

RUN apt-get update \
    && apt-get install -y python3-opencv tesseract-ocr-eng \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*

# COPY requirements.txt .
# RUN pip install -U --no-cache-dir -r requirements.txt

# Install Poetry
RUN curl -sSL https://install.python-poetry.org | python3 -
# Add Poetry to PATH for subsequent commands
ENV PATH="/root/.local/bin:${PATH}"

COPY poetry.toml .
COPY poetry.lock .
COPY pyproject.toml .
RUN poetry install --no-root

COPY . .

EXPOSE 8000

CMD ["python", "app.py"]
