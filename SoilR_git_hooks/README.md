In order to make the hook scripts available to every user of the repo,
we moved them from the standard location `${GIT_DIR}/.git/hooks` 
to this directory which is itself under version control.
To activate the hook scripts you have to configure YOUR INSTANCE of the repoto look for them here instead of the usual `${GIT_DIR}/.git/hooks` location
by entering the following code (once):

```git config core.hooksPath SoilR_git_hooks 
```
from the top directory of the repository
