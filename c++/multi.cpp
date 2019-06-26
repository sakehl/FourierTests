#include <iostream>
#include <cstdlib>
#include <pthread.h>
#include <complex>

using namespace std;

#define NUM_THREADS 4
extern "C" void testingIr(int n, int* segs, int* offset, complex<double>* input, complex<double>* output);
extern "C" void testingR(int n, int width, complex<double>* input, complex<double>* output);


int mainTest () {
    
    int n = 4;
    int w = 5;
    complex<double>* input = new complex<double>[n*w];
    
    for(int i = 0; i < n; i++){
        for(int j = 0; j < w; j++)
            input[i*n + j] = complex<double>(i,j);
    }
    /*
    for(int i = 0; i < n; i++)
        for(int j = 0; j < w; j++)
            cout << i << " " << j << ": " << input[i*n+j] << "\n";
    */
    
    complex<double>* res = new complex<double>[n*w];
    testingR(n,w, input, res);
    cout << "Completed ops" << "\n";
    
    for(int i = 0; i < n; i++)
        for(int j = 0; j < w; j++)
            cout << i << " " << j << ": " << res[i*n+j] << "\n";
    
}

//int main(){mainTest();}


struct thread_data {
    int  n_begin;
    int  n_end;
    int* segs;
    int* offset;
    complex<double>* input;
    complex<double>* output;
};

void *adderThread(void *threadarg){
    struct thread_data *my_data;
    my_data = (struct thread_data *) threadarg;
    
    for(int i = my_data->n_begin; i < my_data->n_end; i++)
    {   //cout << "i: " << i << " offset: " << my_data->offset[i] << " seggs: " << my_data->segs[i] <<"\n";
        for(int j = my_data->offset[i]; j < my_data->offset[i] + my_data->segs[i]; j++)
        {   
            //cout << j << "\n";
            //cout << "(i,j )" << (i,j) << "\n";
            my_data->output[j] = my_data->input[j] + 1.1;
        }
    }
        
    pthread_exit(NULL);
}

extern "C" void testingIr(int n, int* offset, int* segs, complex<double>* input, complex<double>* output){
    /*cout << "testingIr function\n";
    for(int i = 0; i<n;i++){
        cout << "i, segs, offset: " << i << segs[i] << offset[i] << "\n";
    }
    */
    pthread_t threads[NUM_THREADS];
    struct thread_data td[NUM_THREADS];
    int rc;

    int begin = 0;
    int workLoad = n / NUM_THREADS;
    int rest     = n % NUM_THREADS;

    for(int i = 0; i < NUM_THREADS; i++ ) {
        td[i].segs = segs;
        td[i].offset = offset;
        td[i].input = input;
        td[i].output = output;
        td[i].n_begin = begin;
        begin += i < rest ? workLoad + 1 : workLoad;
        td[i].n_end = begin;
        //cout << "n_begin: " << td[i].n_begin << " n_end: " << td[i].n_end << "\n";

        rc = pthread_create(&threads[i], NULL, adderThread, (void *)&td[i]);

        if (rc) {
            cout << "Error:unable to create thread," << rc << endl;
            exit(-1);
        }
    }

    for(int i =0; i <  NUM_THREADS; i++)
    {
        pthread_join(threads[i], NULL);
    }

}

extern "C" void testingR(int n, int width, complex<double>* input, complex<double>* output){
    //cout << "testingR function\n"; 
    pthread_t threads[NUM_THREADS];
    struct thread_data td[NUM_THREADS];
    int rc;

    int begin = 0;
    int workLoad = n / NUM_THREADS;
    int rest     = n % NUM_THREADS;

    int* offset = new int[n];
    int* segs  = new int[n];
    int base = 0;
    for(int i = 0; i < n; i++)
    {
        segs[i] = width;
        offset[i] = base;
        base += width;
        //cout << segs[i] << "<- segs | offset -> " << offset[i] << "\n";
    }

    for(int i = 0; i < NUM_THREADS; i++ ) {
        td[i].segs = segs;
        td[i].offset = offset;
        td[i].input = input;
        td[i].output = output;
        td[i].n_begin = begin;
        begin += i < rest ? workLoad + 1 : workLoad;
        td[i].n_end = begin;
        
        rc = pthread_create(&threads[i], NULL, adderThread, (void *)&td[i]);
        
        if (rc) {
            cout << "Error:unable to create thread," << rc << endl;
            exit(-1);
        }
    }

    for(int i =0; i <  NUM_THREADS; i++)
    {
        pthread_join(threads[i], NULL);
    }
}


extern "C" void testingOne(int l, complex<double>* xs, complex<double>* result){
    cout << "testingOne function\n"; 
    for(int i = 0; i < l; i++)
        result[i] = xs[i] + 1.1;
}