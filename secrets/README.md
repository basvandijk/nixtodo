This directory contains secret files. It would be unsafe to store them as
plaintext so we encrypt them using the [git-crypt][1] tool. Execute the
following command to decrypt these files:

    git-crypt unlock <keyfile>

Get the `<keyfile>` from one of the maintainers who can create it by executing
the following command:

    git-crypt export-key <keyfile>

Note that the `.gitattributes` file in the root of the lumi repository specifies
which files should and shouldn't be encrypted.

[1]: https://www.agwa.name/projects/git-crypt/
