# Use the official VS Code “base” image for Ubuntu
FROM mcr.microsoft.com/vscode/devcontainers/base:ubuntu

# Install system dependencies
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        curl \
        zip \
        unzip \
        git \
        ca-certificates \
        openjdk-17-jdk \
    && rm -rf /var/lib/apt/lists/*

# Install SDKMan into /root/.sdkman (default for the root user)
RUN curl -s "https://get.sdkman.io" | bash

# Make sure sdkman is available in non-interactive steps, then install Scala and sbt
RUN bash -lc "\
    source \$HOME/.sdkman/bin/sdkman-init.sh && \
    sdk install scala 3.3.5 && \
    sdk install sbt 1.10.11 \
"

# Persist SDKMan environment for all shells
RUN echo 'export SDKMAN_DIR=\"\$HOME/.sdkman\"' >> /etc/profile.d/sdkman.sh \
 && echo '[[ -s \"\$HOME/.sdkman/bin/sdkman-init.sh\" ]] && source \"\$HOME/.sdkman/bin/sdkman-init.sh\"' >> /etc/profile.d/sdkman.sh

# Use bash -lc by default so that “source sdkman-init.sh” always works
SHELL ["/bin/bash", "-lc"]
