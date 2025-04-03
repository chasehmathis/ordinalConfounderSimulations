# Ordinal Confounder Simulations

## Recreating Simulations

To recreate the simulations from the paper "A New Method to Adjust for Ordinal Variables", follow the instructions below.
The package for this method is available at <a href="https://github.com/chasehmathis/ordinalconfounder">https://github.com/chasehmathis/ordinalconfounder</a>
### Directions

#### Continuous Assumptions with Linear Model
1. Navigate to the `continuous` directory:
    ```sh
    cd continuous
    ```
2. Run the driver script:
    ```sh
    source driver.sh
    ```
3. Output will be generated in the `SIMS`, `SIMStype1error`, and `figs` directories.

#### GLM Binary
1. Navigate to the `binary` directory:
    ```sh
    cd binary
    ```
2. Run the driver script:
    ```sh
    source driver.sh
    ```
3. Output will be generated in the `SIMS`, `SIMStype1error`, and `figs` directories.

### Notes
- The process should take around 15-20 minutes.
- You can change the number of simulations, seed, and number of samples in the `driver.sh` script.

