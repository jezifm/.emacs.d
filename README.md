# Emacs Configuration

My personal emacs settings.

> People talk about getting used to a new editor, but over time, it is precisely the opposite that should happen - the editor should get used to us.
>
> -- <cite>Vivek Haldar</cite>

## Installation

```sh
cd ~
git clone --recursive git@github.com:jezifm/.emacs.d.git
```


## Extra

### Enable Plantuml

```sh
PLANTUML_JAR_URL='https://github.com/plantuml/plantuml/releases/download/v1.2024.5/plantuml-1.2024.5.jar'; \
    mkdir -p ~/.emacs.d/elpa/contrib/scripts; \
    curl -L -o ~/.emacs.d/elpa/contrib/scripts/plantuml.jar \
    $PLANTUML_JAR_URL
```
