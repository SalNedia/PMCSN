#include <stdio.h>
#include <math.h>
#include "rngs.h"                      /* the multi-stream generator */
#include "rvgs.h"
#include "list.h"
#include "utils.h"
#include "rvms.h"
#include <stdlib.h>
#include <string.h>

#define LOC                 0.95        /* level of confidence, use 0.95 for 95% confidence */
#define START               0.0          /* initial (open the door)        */
#define STOP                86400.0       /* terminal (close the door) time */
#define CLOUDLET_SERVERS    20          /* cloudlet's capcity             */
#define LAMBDA1             4.0          /*     task/second                      */
#define LAMBDA2             6.25         /*     task/second                      */
#define MU1_CLET            0.45         /*     task/second                      */
#define MU1_CLOUD           0.25         /*     task/second                      */
#define MU2_CLET            0.27         /*     task/second                     */
#define MU2_CLOUD           0.22         /*     task/second                     */
#define P                   0.2          /*     probability for Hyperexponential*/
#define TYPE1               1
#define TYPE2               2
#define CLOUD               1
#define CLOUDLET            0
#define INFINITE            (100.0 * STOP) /* must be much larger than STOP  */
#define SEED                123456
#define S                   20
#define BATCH_SIZE          3600
#define SETUP               0.5

typedef struct {                            /* the next-event list    */
    double t;                              /*   next event time      */
    int x;                             /*   event status, 0 or 1 */
    int class;                           /*   class, 1 or 2       */
    int service;                         /*   service time     */
} event_list[CLOUDLET_SERVERS + 2];

typedef struct {
    long N;
    double list_w[9];
    double list_b[9];
    double list_diff[9];
    double list_sum[9];
    double list_mean[9];
    double list_stdev[9];
    double list_value[9];
    double list_IC[9];
    //0=global, 1=clet, 2=cloud, 3=clet1, 4=clet2, 5=cloud1, 6=cloud2, 7=class1, 8=class2;
} statistics;

//FILE* open_file(char* name) {
//
//    FILE * fp;
//    fp = fopen(name, "w");// "w" means that we are going to write on this file
//    if(fp==NULL){
//        perror("error:");
//        exit(-1);
//    }
//    return fp;
//
//}

double GetArrival(int class)
/* ---------------------------------------------
 * generate the next arrival time, with rate lambda
 * ---------------------------------------------
 */
{
    static double arrival1 = START;
    static double arrival2 = START;

    SelectStream(0);
    if (class == TYPE1) {
        arrival1 += Exponential(LAMBDA1);
        return arrival1;
    }
    if (class == TYPE2) {
        arrival2 += Exponential(LAMBDA2);
        return (arrival2);
    }
}

double GetService(int class, int server)
/* ---------------------------------------------
 * generate the next service time, with rate deending by class, NOTE server must be 1 or 0
 * ---------------------------------------------
 */
{
    SelectStream(1);
    if (class == TYPE1) {
        if (server == CLOUDLET) {
            //double d = (Hyperexponential(MU1_CLET, P));
            double d = (Exponential(MU1_CLET));
            return d;
        } else if (server == CLOUD)
            return Exponential(MU1_CLOUD);
    }
    if (class == TYPE2) {
        if (server == CLOUDLET) {
            //double d = (Hyperexponential(MU2_CLET, P));
            double d = (Exponential(MU2_CLET));
            return d;
        } else if (server == CLOUD)
            return Exponential(MU2_CLOUD);
    }
}

double ChangeService(double remainingTime) {
    return (MU2_CLOUD / MU2_CLET) * remainingTime + SETUP;
}

int NextEvent(event_list event, struct node_t **h, struct node_t **selected)
/* ---------------------------------------
 * return the index of the next event type
 * ---------------------------------------
 */
{
    int e;
    int i = 0;

    while (event[i].x == 0 && i < CLOUDLET_SERVERS + 1)
        i++;
    e = i;
    while (i < CLOUDLET_SERVERS + 1) {
        i++;
        if ((event[i].x == 1) && (event[i].t < event[e].t))
            e = i;
    }

    //finché c'è un job nel cloudlet
    struct node_t *p;
    for (p = *h; p != NULL; p = p->next) {
        if (p->x == 1) {
            if (p->t < event[e].t) {
                *selected = p;
                return -1;
            }

        }

    }
    if (e == 0)
        event[e].class = TYPE1;
    if (e == 1)
        event[e].class = TYPE2;

    return e;
}

int FindOne(event_list event)
/* -----------------------------------------------------
 * return the index of the available server idle longest
 * -----------------------------------------------------
 */
{
    int s;
    int i = (int) (Random() * 100) % (CLOUDLET_SERVERS) + 2;

    while (event[i].x == 1) {     /* find the index of the first available */
        i++;                        /* (idle) server                         */
        //Se è arrivato alla fine ricomincia a cercare dal primo server
        if (i > CLOUDLET_SERVERS + 1) {
            i = 2;
        }
    }
    s = i;
    if (s < 2 || s > CLOUDLET_SERVERS + 2)
        printf("ERRORE s=%d", s);
    return (s);
}

//Trova il task di classe 2 da prelazionare
int FindClass2(event_list event) {
    for (int i = 2; i < CLOUDLET_SERVERS + 2; i++) {     /* find the index of the first of class 2*/
        if (event[i].class == 2) {
            return i;
        }
    }
    return -1;
}

void computeStats(statistics *st) {
    statistics p = *st;
    p.N++;
    //calcolo tempi di risposta dei vari casi(cloudlet,cloud,...)
    for (int i = 0; i < 9; ++i) {
        //welford algorithm
        p.list_diff[i] = p.list_value[i] - p.list_mean[i];
        p.list_sum[i] += p.list_diff[i] * p.list_diff[i] * (p.N - 1.0) / p.N;
        p.list_mean[i] += p.list_diff[i] / p.N;
        p.list_stdev[i] = sqrt(p.list_sum[i] / p.N);
    }
    *st = p;
}

void computeIC(statistics *st) {
    double u, t_star, w;
    statistics p = *st;
    //calcolo intervallo di confidenza
    for (int i = 0; i < 9; ++i) {
        if (p.N > 1) {
            u = 1.0 - 0.5 * (1.0 - LOC);                                    /* interval parameter  */
            t_star = idfStudent((long) p.N - 1, u);                       /* critical value of t */
            w = t_star * p.list_stdev[i] / sqrt(p.N - 1);               /* interval half width */
            p.list_IC[i] = w;
        }
    }
    *st = p;
}

//Aggiornamento di alcune statistiche
void updateStats(statistics *st, int one, int two, int three, int f, double service) {
    statistics p = *st;

    p.list_w[one] += service;
    p.list_w[two] += service;
    p.list_w[three] += service;
    p.list_w[f] += service;

    p.list_b[one]++;
    p.list_b[two]++;
    p.list_b[three]++;
    p.list_b[f]++;

    p.list_value[one] = p.list_w[one] / p.list_b[one];
    p.list_value[two] = p.list_w[two] / p.list_b[two];
    p.list_value[three] = p.list_w[three] / p.list_b[three];
    p.list_value[f] = p.list_w[f] / p.list_b[f];

    *st = p;
}

//Aggiornamento di tutte le statistiche
void updateAllStats(statistics *st, double *values) {
    statistics p = *st;
    for (int i = 0; i < 9; ++i) {
        p.list_w[i] += values[i];
        p.list_b[i]++;
        p.list_value[i] = p.list_w[i] / p.list_b[i];
    }
    *st = p;
}

int main(void) {

    event_list event;
    statistics rt, population, thr;

    //inizializzazione strutture statistiche
    memset(&rt, 0, sizeof rt);
    memset(&population, 0, sizeof population);
    memset(&thr, 0, sizeof thr);
    struct node_t *cloud_servers = NULL;
    struct node_t *sel;
    size_t num_batch = (size_t) STOP / BATCH_SIZE;

    struct {
        double current;                  /* current time                       */
        double next;                     /* next (most imminent) event time    */
    } t;

    long index = 0;     //numero di completamenti
    long num_cloudlet = 0;//popolazione del cloudlet
    long num_cloud = 0; //popolazione del cloud
    int n1Clet = 0; // completamenti del cloudlet classe 1
    int n2Clet = 0; //completamenti del cloudlet classe 2
    int n1Cloud = 0; //completamenti del cloud classe 1
    int n2Cloud = 0; //completamenti del cloud classe 2
    int m1Clet = 0; // popolazione del cloudlet classe 1
    int m2Clet = 0; //popolazione del cloudlet classe 2
    int m1Cloud = 0; //popolazione del cloud classe 1
    int m2Cloud = 0; //popolazione del cloud classe 2

    int s = 0;
    int e;
    int c = 1;
    double prev_current = 0.0;          /* precedent current time */
    double prev_next = 0.0;              /* precedent current time */
    double inizio = 0.0;
    double fine = 0.0;

    struct {                           /* accumulated sums of                */
        double service;                  /*   service times                    */
        long served;                   /*   number served                    */
    } sum[CLOUDLET_SERVERS + 2];

    //Inizializzazione per la Next Event Simulation
    PlantSeeds(SEED);
    t.current = START;
    t.next = START;
    event[0].t = GetArrival(TYPE1);
    event[0].x = 1;
    event[0].class = TYPE1;
    event[1].t = GetArrival(TYPE2);
    event[1].x = 1;
    event[1].class = TYPE2;

    for (s = 2; s <= CLOUDLET_SERVERS + 1; s++) {
        event[s].t = START;          /* this value is arbitrary because */
        event[s].x = 0;              /* all servers are initially idle  */
        sum[s].service = 0.0;
        sum[s].served = 0;
    }

//    FILE *fdt = open_file("throughput.txt");
//    FILE *fdt1 = open_file("throughput1.txt");
//    FILE *fdtClet1 = open_file("throughputClet1.txt");
//    FILE *fdtClet2 = open_file("throughputClet2.txt");
//    FILE *fdt2 = open_file("throughput2.txt");
//    FILE *fdtclet = open_file("throughputClet.txt");
//    FILE *fdtcloud = open_file("throughputCloud.txt");
//    FILE *fdtcloud1 = open_file("throughputCloud1.txt");
//    FILE *fdtcloud2 = open_file("throughputCloud2.txt");
//    FILE *fde = open_file("responseTime.txt");
//    FILE *fde1 = open_file("responseTime1.txt");
//    FILE *fdeClet1 = open_file("responseTimeClet1.txt");
//    FILE *fdeClet2 = open_file("responseTimeClet2.txt");
//    FILE *fde2 = open_file("responseTime2.txt");
//    FILE *fdeclet = open_file("responseTimeClet.txt");
//    FILE *fdecloud = open_file("responseTimeCloud.txt");
//    FILE *fdecloud1 = open_file("responseTimeCloud1.txt");
//    FILE *fdecloud2 = open_file("responseTimeCloud2.txt");
//    FILE *fda = open_file("interarrivi.txt");
//
//    int CloudClass;

    double jobPrelazionati = 0.0;
    double TempoRimanenteMedia = 0.0;
    double PrevArr = 0.0;

    while ((event[0].x != 0) || event[1].x != 0 || (m1Clet > 0) || (m2Clet > 0) || m1Cloud > 0 || m2Cloud > 0) {
        e = NextEvent(event, &cloud_servers, &sel);
        t.next = event[e].t;
        t.current = t.next;
        if (e == 0) {
            //evento arrivo job classe 1
            event[0].t = GetArrival(1);
            if (event[0].t > STOP)
                event[0].x = 0;
            else if (num_cloudlet < CLOUDLET_SERVERS) { // if(n1+n2<S)
                //popolazione cloudlet < S, allora accetto sia job1 che job2
                num_cloudlet++;
                m1Clet++;
                double service = GetService(TYPE1, CLOUDLET);
                s = FindOne(event);
                event[s].class = TYPE1;
                event[s].service = service;
                event[s].t = t.current + service;
                event[s].x = 1;
                sum[s].service += service;
                sum[s].served++;

                //aggiornamento valori tempi di risposta
                updateStats(&rt, 0, 1, 3, 7, service);

//                fprintf(fdtclet, "%f ; %f;\n", t.current, thr.list_value[1]);
//                fprintf(fdtClet1, "%f ; %f;\n", t.current, thr.list_value[3]);
//                fprintf(fdt1, "%f ; %f; \n", t.current, thr.list_value[7]);
//
//                fprintf(fdeclet, "%f ; %f;\n", t.current, rt.list_value[1]);
//                fprintf(fdeClet1, "%f ; %f;\n", t.current, rt.list_value[3]);
//                fprintf(fde1, "%f ; %f;\n", t.current, rt.list_value[7]);
            } else if (num_cloudlet >= S) {
                int prelazione = FindClass2(event);
                if (prelazione == -1) { //caso n1=N
                    //non ci sono job di classe 2 nel clet => job1 devono andare necessariamente nel cloud
                    num_cloud++;
                    m1Cloud++;
                    double service = GetService(TYPE1, CLOUD);
                    insert_node(TYPE1, t.current + service, &cloud_servers);

                    //aggiornamento valori tempi di risposta
                    updateStats(&rt, 0, 2, 5, 7, service);

//                    fprintf(fdtcloud, "%f ; %f;\n", t.current, thr.list_value[2]);
//                    fprintf(fdtcloud1, "%f ; %f;\n", t.current, thr.list_value[5]);
//                    fprintf(fdt1, "%f ; %f; \n", t.current, thr.list_value[7]);
//
//                    fprintf(fdecloud, "%f ; %f;\n", t.current, rt.list_value[2]);
//                    fprintf(fdecloud1, "%f ; %f;\n", t.current, rt.list_value[5]);
//                    fprintf(fde1, "%f ; %f;\n", t.current, rt.list_value[7]);
//
//                    fprintf(fda, "%f\n", (t.current - PrevArr));
                    PrevArr = t.current;
                } else if ((prelazione >= 2) && (prelazione <= CLOUDLET_SERVERS + 2)) { //caso n2>0
                    jobPrelazionati++;
                    num_cloudlet--;
                    m2Clet--;
                    num_cloud++;
                    m2Cloud++;

                    //TempoRimanenteMedia += (event[prelazione].t - t.current);
                    //double newTime = ChangeService(event[prelazione].t-t.current);
                    double newTime = GetService(TYPE2, CLOUD) + SETUP;
                    insert_node(TYPE2, t.current + newTime, &cloud_servers);

                    //aggiornamento valori tempi di risposta
                    updateStats(&rt, 0, 2, 6, 8, newTime);

//                    fprintf(fdtcloud, "%f ; %f;\n", t.current, thr.list_value[2]);
//                    fprintf(fdtcloud2, "%f ; %f;\n", t.current, thr.list_value[6]);
//                    fprintf(fdt2, "%f ; %f; \n", t.current, thr.list_value[8]);
//
//                    fprintf(fdecloud, "%f ; %f;\n", t.current, rt.list_value[2]);
//                    fprintf(fdecloud2, "%f ; %f;\n", t.current, rt.list_value[6]);
//                    fprintf(fde2, "%f ; %f;\n", t.current, rt.list_value[8]);
//
//                    fprintf(fda, "%f\n", (t.current - PrevArr));
                    PrevArr = t.current;

                    //METTO UN EVENTO CLASSE1 AL SUO POSTO
                    num_cloudlet++;
                    m1Clet++;
                    double service = GetService(TYPE1, CLOUDLET);
                    event[prelazione].class = TYPE1;
                    event[prelazione].t = t.current + service;
                    event[prelazione].x = 1;
                    sum[prelazione].service += service;
                    sum[prelazione].served++;

                    //aggiornamento valori tempi di risposta
                    updateStats(&rt, 0, 1, 3, 7, service);

//                    fprintf(fdtclet, "%f ; %f;\n", t.current, thr.list_value[1]);
//                    fprintf(fdtClet1, "%f ; %f;\n", t.current, thr.list_value[3]);
//                    fprintf(fdt1, "%f ; %f; \n", t.current, thr.list_value[7]);
//
//                    fprintf(fdeclet, "%f ; %f;\n", t.current, rt.list_value[1]);
//                    fprintf(fdeClet1, "%f ; %f;\n", t.current, rt.list_value[3]);
//                    fprintf(fde1, "%f ; %f;\n", t.current, rt.list_value[7]);
                }
            }
        }
        if (e == 1) {
            //evento arrivo job classe 2
            event[1].t = GetArrival(TYPE2);
            if (event[1].t > STOP)
                event[1].x = 0;
            else if (num_cloudlet < S) {
                num_cloudlet++;
                m2Clet++;

                double service = GetService(TYPE2, CLOUDLET);
                s = FindOne(event);
                event[s].class = TYPE2;
                event[s].service = service;
                event[s].t = t.current + service;
                event[s].x = 1;

                sum[s].service += service;
                sum[s].served++;

                //aggiornamento valori tempi di risposta
                updateStats(&rt, 0, 1, 4, 8, service);

//                fprintf(fdtclet, "%f ; %f;\n", t.current, thr.list_value[1]);
//                fprintf(fdtClet2, "%f ; %f;\n", t.current, thr.list_value[4]);
//                fprintf(fdt2, "%f ; %f; \n", t.current, thr.list_value[8]);
//
//                fprintf(fdeclet, "%f ; %f;\n", t.current, rt.list_value[1]);
//                fprintf(fdeClet2, "%f ; %f;\n", t.current, rt.list_value[4]);
//                fprintf(fde2, "%f ; %f;\n", t.current, rt.list_value[8]);
            } else if (num_cloudlet >= S) {
                num_cloud++;
                m2Cloud++;

                double service = GetService(TYPE2, CLOUD);
                insert_node(TYPE2, t.current + service, &cloud_servers);

                //aggiornamento valori tempi di risposta
                updateStats(&rt, 0, 2, 6, 8, service);

//                fprintf(fdtcloud, "%f ; %f;\n", t.current, thr.list_value[2]);
//                fprintf(fdtcloud2, "%f ; %f;\n", t.current, thr.list_value[6]);
//                fprintf(fdt2, "%f ; %f; \n", t.current, thr.list_value[8]);
//
//                fprintf(fdecloud, "%f ; %f;\n", t.current, rt.list_value[2]);
//                fprintf(fdecloud2, "%f ; %f;\n", t.current, rt.list_value[6]);
//                fprintf(fde2, "%f ; %f;\n", t.current, rt.list_value[8]);
//
//                fprintf(fda, "%f\n", (t.current - PrevArr));
                PrevArr = t.current;
            }
        }
        if (e > 1) {
            //evento partenza da cloudlet
            index++;
            num_cloudlet--;
            if (event[e].class == TYPE1) {
                m1Clet--;
                n1Clet++;
            }
            if (event[e].class == TYPE2) {
                m2Clet--;
                n2Clet++;
            }
            event[e].x = 0;        //set to idle
            event[e].t = INFINITE; //set to unachievable
        }
        if (e == -1) {
            //evento partenza dal cloud
            index++;
            num_cloud--;
//            //PER FUNZIONARE MI SERVONO QUESTE DUE RIGHE!
//            t.current = sel->t;
//            CloudClass = sel->class;

            if (sel->class == TYPE1) {
                m1Cloud--;
                n1Cloud++;
                clear_node(sel->index, &cloud_servers);
            }
            if (sel->class == TYPE2) {
                m2Cloud--;
                n2Cloud++;
                clear_node(sel->index, &cloud_servers);
            }
        }

        if (t.current >= c * BATCH_SIZE) {
            long seed;
            GetSeed(&seed);
            PutSeed(seed);

            computeStats(&rt);
            computeStats(&thr);
            computeStats(&population);
            c++;
        }

        if (t.current < STOP) {

            double thr_values[9] = {index / t.current, (n1Clet + n2Clet) / t.current, (n1Cloud + n2Cloud) / t.current,
                                    n1Clet / t.current,
                                    n2Clet / t.current, n1Cloud / t.current, n2Cloud / t.current,
                                    (n1Clet + n1Cloud) / t.current, (n2Clet + n2Cloud) / t.current};

            double pop_values[9] = {(m1Clet + m2Clet + m1Cloud + m2Cloud), (m1Clet + m2Clet), (m1Cloud + m2Cloud),
                                    m1Clet,
                                    m2Clet, m1Cloud, m2Cloud, (m1Clet + m1Cloud), (m2Clet + m2Cloud)};

            //aggiornamento valori throughput
            updateAllStats(&thr, thr_values);
            //aggiornamento valori popolazione
            updateAllStats(&population, pop_values);
//
//            fprintf(fde, "%f ; %f\n", t.current, rt.list_value[0]);
//            fprintf(fdt, "%f ; %f\n", t.current, thr.list_value[0]);
        }

    }

    //calcolo degli intervalli di confidenza per i response time
    computeIC(&rt);
    //calcolo degli intervalli di confidenza per le popolazioni
    computeIC(&population);
    //calcolo degli intervalli di confidenza per i throughput
    computeIC(&thr);

//
//    fclose(fde);
//    fclose(fde);

//    printf("\nPARAMETRO: %f\n", (TempoRimanenteMedia / jobPrelazionati));
//
//    printf("\n\nPERCENTUALE: %f, n1clet: %d, n2clet: %d, n1cloud: %d, n2cloud: %d\n",
//           (jobPrelazionati / (n2Clet + jobPrelazionati)), n1Clet, n2Clet, n1Cloud, n2Cloud);
//
//    printf("welford global response time..... .mean:%f (stdev:%f) in the interval+/-%6.5f\n", rt.list_mean[0],
//           rt.list_stdev[0], rt.list_IC[0]);
//    printf("welford clet response time........ mean:%f (stdev:%f) in the interval+/-%6.5f\n", rt.list_mean[1],
//           rt.list_stdev[1], rt.list_IC[1]);
//    printf("welford cloud response time....... mean:%f (stdev:%f) in the interval+/-%6.5f\n", rt.list_mean[2],
//           rt.list_stdev[2], rt.list_IC[2]);
//    printf("welford clet1 response time....... mean:%f (stdev:%f) in the interval+/-%6.5f\n", rt.list_mean[3],
//           rt.list_stdev[3], rt.list_IC[3]);
//    printf("welford clet2 response time....... mean:%f (stdev:%f) in the interval+/-%6.5f\n", rt.list_mean[4],
//           rt.list_stdev[4], rt.list_IC[4]);
//    printf("welford cloud1 response time...... mean:%f (stdev:%f) in the interval+/-%6.5f\n", rt.list_mean[5],
//           rt.list_stdev[5], rt.list_IC[5]);
//    printf("welford cloud2 response time...... mean:%f (stdev:%f) in the interval+/-%6.5f\n", rt.list_mean[6],
//           rt.list_stdev[6], rt.list_IC[6]);
//    printf("welford class1 response time...... mean:%f (stdev:%f) in the interval+/-%6.5f\n", rt.list_mean[7],
//           rt.list_stdev[7], rt.list_IC[7]);
//    printf("welford class2 response time...... mean:%f (stdev:%f) in the interval+/-%6.5f\n", rt.list_mean[8],
//           rt.list_stdev[8], rt.list_IC[8]);
//
//    //POP
//    printf("\n\nwelford global population..... .mean:%f (stdev:%f) in the interval+/-%6.5f\n", population.list_mean[0],
//           population.list_stdev[0], population.list_IC[0]);
//    printf("welford clet population........ mean:%f (stdev:%f) in the interval+/-%6.5f\n", population.list_mean[1],
//           population.list_stdev[1], population.list_IC[1]);
//    printf("welford cloud population....... mean:%f (stdev:%f) in the interval+/-%6.5f\n", population.list_mean[2],
//           population.list_stdev[2], population.list_IC[2]);
//    printf("welford clet1 population....... mean:%f (stdev:%f) in the interval+/-%6.5f\n", population.list_mean[3],
//           population.list_stdev[3], population.list_IC[3]);
//    printf("welford clet2 population....... mean:%f (stdev:%f) in the interval+/-%6.5f\n", population.list_mean[4],
//           population.list_stdev[4], population.list_IC[4]);
//    printf("welford cloud1 population...... mean:%f (stdev:%f) in the interval+/-%6.5f\n", population.list_mean[5],
//           population.list_stdev[5], population.list_IC[5]);
//    printf("welford cloud2 population...... mean:%f (stdev:%f) in the interval+/-%6.5f\n", population.list_mean[6],
//           population.list_stdev[6], population.list_IC[6]);
//    printf("welford class1 population...... mean:%f (stdev:%f) in the interval+/-%6.5f\n", population.list_mean[7],
//           population.list_stdev[7], population.list_IC[7]);
//    printf("welford class2 population...... mean:%f (stdev:%f) in the interval+/-%6.5f\n", population.list_mean[8],
//           population.list_stdev[8], population.list_IC[8]);
//
//    //throughput
//    printf("\n\nwelford global throughput..... .mean:%f (stdev:%f) in the interval+/-%6.5f\n", thr.list_mean[0],
//           thr.list_stdev[0], thr.list_IC[0]);
//    printf("welford clet throughput........ mean:%f (stdev:%f) in the interval+/-%6.5f\n", thr.list_mean[1],
//           thr.list_stdev[1], thr.list_IC[1]);
//    printf("welford cloud throughput....... mean:%f (stdev:%f) in the interval+/-%6.5f\n", thr.list_mean[2],
//           thr.list_stdev[2], thr.list_IC[2]);
//    printf("welford clet1 throughput....... mean:%f (stdev:%f) in the interval+/-%6.5f\n", thr.list_mean[3],
//           thr.list_stdev[3], thr.list_IC[3]);
//    printf("welford clet2 throughput....... mean:%f (stdev:%f) in the interval+/-%6.5f\n", thr.list_mean[4],
//           thr.list_stdev[4], thr.list_IC[4]);
//    printf("welford cloud1 throughput...... mean:%f (stdev:%f) in the interval+/-%6.5f\n", thr.list_mean[5],
//           thr.list_stdev[5], thr.list_IC[5]);
//    printf("welford cloud2 throughput...... mean:%f (stdev:%f) in the interval+/-%6.5f\n", thr.list_mean[6],
//           thr.list_stdev[6], thr.list_IC[6]);
//    printf("welford class1 throughput...... mean:%f (stdev:%f) in the interval+/-%6.5f\n", thr.list_mean[7],
//           thr.list_stdev[7], thr.list_IC[7]);
//    printf("welford class2 throughput...... mean:%f (stdev:%f) in the interval+/-%6.5f\n", thr.list_mean[8],
//           thr.list_stdev[8], thr.list_IC[8]);
//
//
//    printf("\nfor %ld jobs the service node statistics are:\n\n", index);
//
//
//    //printf("p1clet:%d, p2clet:%d, p1cloud:%d p2cloud:%d\n",m1Clet,m2Clet,m1Cloud,m2Cloud);
//    printf("completamenti 1:%d ; completamenti 2:%d \n", n1Clet + n1Cloud, n2Clet + n2Cloud);
//
////    printf( "THROUGHPUT GLOBALE: %f\n", index/t.current);
////    printf( "THROUGHPUT 1  %f\n", (n1Clet+n1Cloud)/t.current);
////    printf( "THROUGHPUT 2  %f\n", (n2Clet+n2Cloud)/t.current);
////    printf( "THROUGHPUT CLET1  %f\n",  (n1Clet)/t.current);
////    printf( "THROUGHPUT CLET2  %f\n",  (n2Clet)/t.current);
////    printf( "THROUGHPUT CLET  %f\n", (n1Clet+n2Clet)/t.current);
////    printf( "THROUGHPUT CLOUD1  %f\n", (n1Cloud)/t.current);
////    printf( "THROUGHPUT CLOUD2  %f\n", (n2Cloud)/t.current);
////    printf( "THROUGHPUT CLOUD  %f\n", (n1Cloud+n2Cloud)/t.current);
//
//
///*
//    printf("\nthe cloudlet server statistics are:\n\n");
//    printf("    server     utilization     avg service\n");
//    double avgs[CLOUDLET_SERVERS];
//    for (s = 2; s <= CLOUDLET_SERVERS+1; s++){
//        if(sum[s].served !=0)
//            avgs[s-2]=sum[s].service / sum[s].served;
//        else
//            avgs[s-2]=0.0;
//    }
//    for (s = 2; s <= CLOUDLET_SERVERS+1; s++)
//        printf("%8d %14.3f %15.2f\n", s-1, sum[s].service / t.current, avgs[s-2]);
//    printf("\n");
//
//*/
    return (0);
}



