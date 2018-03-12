# dplyr archive

A set of packrats for various dplyr versions, connected to MRAN to be able to collect the dependencies in the correct version. The package sources are not under version control, we need MRAN to install a snapshot.

## Test code with a particular dplyr version

1. Change to a directory
1. Start R
    - If the packrat hasn't been initialized yet, it will do now
1. Run the code

Example:

```sh
cd archive/0.7.4-earliest
R -q -e 'dplyr::slice(c(-1, 1))'
```

## Add a new packrat

1. Change to the `archive` directory.
1. Use the `add-version.sh` script.

Example: dplyr 0.7.5 has been released on March 16, 2018. We add dplyr 0.7.4 snapshotted on March 14, and dplyr 0.7.5 on March 18:

```sh
./add-version.sh 0.7.4-latest 2018-03-14
./add-version.sh 0.7.5-earliest 2018-03-18
```
