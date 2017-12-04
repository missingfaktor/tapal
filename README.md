# Tapal

Tapal is currently a dreamware, which is to say, we have only thought about it, but have not yet built it. What follows is a raw dump of our vision and ideas.

Tapal aims to be a lightweight command line alternative to Postman.

Postman has a wide feature set. The ones that are most important to us are:
1. A repository of network API examples that is shared across teams, readable and modifiable by team members.
2. Scripts executed after API requests. These are typically used to set environment variables.
3. Scopes. These are typically used to manage different environments, such as Dev, QA, Prod.
4. Variables. These can be used to save domain-specific entities, reducing need for constant copy-pasting across various API call probes. These are also scoped.

Tapal aims to provide all of the above features.

Tapal will be a Git-backed tool, with an intuitive, well-specified format for organising data in directories and files (configurations, scripts etc). Using Git means we get an excellent distributed, decentralised, versioned storage for free.

Tapal will be designed with inspectability and discovery in mind. As such there will be commands for inspecting the key elements of the system, such as `tapal variables`, `tapal scopes` etc. You will also have auto-completion wherever it makes sense.

We might consider putting together a graphical front-end together too, but that can happen only after the command line version is feature complete.
