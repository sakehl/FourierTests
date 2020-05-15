# Artifact for Euro-Par 2020 paper `Accelerating Nested Data Parallelism: Preserving Regularity`
This is the source code we used to do the experiments of Section 5 (Evaluation). More specifically you can reproduce the four figures of Figure 3 with this code.
The repository can be found [here](https://github.com/sakehl/FourierTests/tree/artifact)
it can be cloned with
```bash
git clone --single-branch -b artifact https://github.com/sakehl/FourierTests.git
```
**NOTE You do not have to clone this repository for running everything, it is also included in the docker image. But if you want to inspect the code we use, this repo is more usefull than a docker image.**

## Getting started guide
### Requirements
  * A linux machine (only tested with Ubuntu 18.04)
  * Nvidia GPU, atleast 11GB of memory is needed to run all experiments. (Less memory is possible, but some experiments will run out of memory)
  * Atleast nvidia driver version >= 418.39 must be installed.
  * CUDA toolkit is not needed, comes with the docker.

### Installation
Install the following
  * [Docker](https://docs.docker.com/install/)
  * [Nvidia's Docker runtime](https://github.com/NVIDIA/nvidia-docker#quickstart)

We need a folder on the host, so you can easily see the produced (intermediate) results. In the folder where you are going to run the docker image and experiments, make a `data` folder.
E.g.
```bash
mkdir ~/euro-par-20-accelerate
cd ~/euro-par-20-accelerate
mkdir data
```
Now do the following
```bash
sudo docker run --gpus all -it --privileged --mount type=bind,source="$(pwd)"/data,target=/root/FourierTests/data lvandenhaak/accelerate-euro-par-20
```
And see if everything is working.

## Step-by step instruction on how to reproduce the results
**NOTE: If you want to shorten the experiments, read the next section first. Otherwise the runs might take up to 3-5 hours.**
The original experiments were conducted on a GeForce RTX 2080Ti (compute capability 7.0, 68 multiprocessors = 4352 cores at 1.65GHz, 11GB RAM) backed on by 16-core Threadripper 2950X (1.9GHz, 64GB RAM).
1. Start the docker image
    ```bash
    sudo docker run --gpus all -it --privileged --mount type=bind,source="$(pwd)"/data,target=/root/FourierTests/data lvandenhaak/accelerate-euro-par-20
    ```
1. To reproduce the quicksort results do (inside the docker bash)
    ```bash 
    ./make_quicksort_dat.sh
    ```
    **Note this can take up a long time, on our machine about 2 to 3 hours. Mostly busy compiling Accelerate's Irregular version.**
1. Inspect the results in the data folder on your host. `quicksort-100.pdf`, `quicksort-1000.pdf` and `quicksort-10000.pdf` should be created and look similar to figure 3 of the paper. The files `quicksort-100.dat` contain the raw data points. The files `result_quicksort_Regular_100_1.csv` contain the output we get from the nvidia profiler (nvprof)
1. To reproduce the fourier results do (inside the docker bash)
    ```bash 
    ./make_fourier_dat.sh
    ```
    **Note also takes a long time, about 1 to 2 hours on our machine**
1. Inspect the results in the data folder on your host. `fourier32x32.pdf`, `quicksort-1000.pdf` and `quicksort-10000.pdf` should be created and look similar to figure 3 of the paper.

### Shortening the runs
We've provided some arguments to the above bash scripts, to run shorter versions, although not all figures will be produced in that case. This can be run with
```bash 
./make_quicksort_dat.sh --short
./make_fourier_dat.sh --short
```
### Run specific benchmark again
To run a specific benchmark again, you can give some other arguments, e.g.
```bash
./make_quicksort_dat.sh --regular -m 100 -n 1
./make_fourier_dat.sh --futhark -n 1
```
For quicksort, valid values for m are 100, 1000 and 1000 (the three differunt subgraphs). Valid values for n are 1, 100, 1000, 2000, 5000, 10000. Valid versions (as indicated by the legend of Figure 3) are --regular (Accelerate, Regular) --irregular (Accelerate), --futhark (Futhark).

For fourier, valid values for n are 1, 100, 1000, 5000, 10000, 20000. Valid versions (as indicated by the legend of Figure 3) are --regular (Accelerate, Regular) --irregular (Accelerate), --futhark (Futhark), --cufft (cuFFT) and --normal (Normal).

### Out of memory problems
Unfortunately, processes that use the maximum ammount of memory (or nearly so) can stall a long time. You can just cancel a specific benchmark with `[ctrl] + c`. It will give a warning that it isn't included in the .dat file, but it should still process fine.