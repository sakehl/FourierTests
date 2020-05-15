FROM nvidia/cuda:10.1-devel-ubuntu18.04

WORKDIR /root

ARG DEBIAN_FRONTEND=noninteractive
ARG PREFIX=/opt/accelerate-llvm

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8
ENV PATH /root/.cabal/bin:/root/.local/bin:${PATH}
ENV LD_LIBRARY_PATH /usr/local/cuda/lib64:/usr/local/cuda/nvvm/lib64:${LD_LIBRARY_PATH}
ENV LIBRARY_PATH /usr/local/cuda/lib64:/usr/local/cuda/nvvm/lib64:${LIBRARY_PATH}
ENV CPATH /usr/local/cuda/include:${CPATH}
RUN ln -s /usr/local/cuda/lib64/stubs/libcuda.so /usr/local/cuda/lib64/libcuda.so.1

RUN apt-get update \
 && apt-get install -y software-properties-common git

RUN add-apt-repository -y ppa:hvr/ghc \
 && apt-get update \
 && apt-get install -y curl netbase pkg-config wget libfftw3-dev

RUN curl -sSL https://get.haskellstack.org/ | sh

# Buggy versions of ld.bfd fail to link some Haskell packages:
# https://sourceware.org/bugzilla/show_bug.cgi?id=17689.
RUN update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20
RUN update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10

# Install LLVM
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
 && add-apt-repository "deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic main" \
 && apt-get update \
 && apt-get install -y llvm-9-dev

# Fix libffi bug
RUN ln /usr/lib/x86_64-linux-gnu/libffi.so.6 /usr/lib/x86_64-linux-gnu/libffi.so.7

# Install gnuplot
RUN apt install -y libtinfo-dev libgmp-dev gnuplot

# Get the benchmark code for Accelerate and Futhark
RUN git clone --single-branch -b artifact https://github.com/sakehl/FourierTests.git
WORKDIR /root/FourierTests

#Setup Accelerate
RUN stack build

# Install Futhark
RUN apt install -y libtinfo-dev libgmp-dev
ARG PREFIX=/
RUN wget https://futhark-lang.org/releases/futhark-0.13.2-linux-x86_64.tar.xz && tar -xf futhark-0.13.2-linux-x86_64.tar.xz
WORKDIR /root/FourierTests/futhark-0.13.2-linux-x86_64
RUN make install
WORKDIR /root/FourierTests

RUN git pull
RUN stack build

# Install some utilities
RUN apt-get install -y python3 nano

RUN chmod +x make_fourier_dat.sh
RUN chmod +x make_quicksort_dat.sh

RUN rm /usr/local/cuda/lib64/libcuda.so.1

CMD ["bash"]