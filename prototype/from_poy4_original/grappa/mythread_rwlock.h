/* $Id: mythread_rwlock.h 48 2005-04-13 15:56:25Z ron $
   Written by Adam Siepel, Spring 2001 
   Copyright 2001, Adam Siepel */

/* Simple implementation of reader-writer locks that uses pthread
   mutexes and POSIX semaphores.  Interface is modeled on that of the
   "pthread_rwlock" functions, which seem not to be widely supported
   (otherwise I would have used them directly).  Note that the
   implementation here gives priority to readers (i.e., there is a
   possibility of starvation of writers).  */


#ifndef WINNT
#include <pthread.h>
#include <semaphore.h>
#endif

typedef struct
{
    pthread_mutex_t countermutex;
    sem_t writesem;
    int readcount;
} mythread_rwlock_t;

int mythread_rwlock_init ( mythread_rwlock_t * rwlock );

int mythread_rwlock_wrlock ( mythread_rwlock_t * rwlock );

int mythread_rwlock_rdlock ( mythread_rwlock_t * rwlock );

int mythread_rwlock_wrunlock ( mythread_rwlock_t * rwlock );

int mythread_rwlock_rdunlock ( mythread_rwlock_t * rwlock );
