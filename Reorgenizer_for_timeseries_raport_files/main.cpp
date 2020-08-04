#include <iostream>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>


using namespace std;

// Program do analizy danych w plikach cim i generowania plików txt poszczególnych typów obiektów. Autor DDB

#define MAXLINELENGTH 2048  // maksymalna d³ugoœæ pojedynczego wczytywanego wiersza pliku 
#define MAXRP 1000 // maksymalna liczba punktów pomiarowych - stacji SN/nn - w pliku znalaz³em oko³o 500
#define MAXRPD 30 // maksymalna d³ugoœæ nazwy punktu pomiarowego - stacji SN/nn

#define MAXK 10  // maksymalna liczba kolumn
//#define MAXKD 30 // maksymalna d³ugoœæ nazwy kolumny

#define MAXPL 12 // maksymalna liczba plików z danymi 
#define MAXDATETIME 40000 //maksymalna liczba timestampów obs³ugiwanych przez program

int getdata(char *line,char *dats,int poz, char separ);
long dl_pliku_otw(FILE *fp); // podaje liczbe wierszy w pliku
void zam_zn_dz(char *tekst);
void zam_spac_sr(char *tekst, char *buf);

int isalnumcd (int ch); // isalnum + colon + dot

size_t indeks(long stac,long tim, long kol); // funkcja do zwracania wartoœci indeksu w du¿ej tablicy 3 wymiarowej


int analiz_plik(FILE *ami, FILE *log, long dlpl, int & min_ts);
int wczytaj_plik(FILE *ami, FILE *log, long dlpl, int & min_ts);

int print_all;

int li_stacji;
int li_kolumn;
int li_f_e;
int li_timestamp;
char stacje[MAXRP][MAXRPD];
char kolumny[MAXK][MAXRPD];
int  f_e[MAXK];
char timestamp[MAXDATETIME][23];
//char timestamp_fe[MAXDATETIME];
float max_val, min_val;


float *dane;


int main(int argc, char** argv) 
{
FILE *log;
FILE *ami;
FILE *txt;

long i,j,k,i1,j1,k1;
long s,t;
int npl;
int max_ob;
int flag_ob;
int min_ts; // zmienna pozwalaj¹ca na przegl¹danie tylko bie¿¹cego pliku jeœli chodzi o daty

print_all=1;


li_stacji=0;
li_kolumn=0;
li_f_e=0;
li_timestamp=0;
max_val=-1e10;
min_val= 1e10;


char nazwa_pliku_ami[MAXPL][50];
strcpy(nazwa_pliku_ami[0],"Jaw_2019-01.txt");
strcpy(nazwa_pliku_ami[1],"Jaw_2019-02.txt");
strcpy(nazwa_pliku_ami[2],"Jaw_2019-03.txt");
strcpy(nazwa_pliku_ami[3],"Jaw_2019-04.txt");
strcpy(nazwa_pliku_ami[4],"Jaw_2019-05.txt");
strcpy(nazwa_pliku_ami[5],"Jaw_2019-06.txt");
strcpy(nazwa_pliku_ami[6],"Jaw_2019-07.txt");
strcpy(nazwa_pliku_ami[7],"Jaw_2019-08.txt");
strcpy(nazwa_pliku_ami[8],"Jaw_2019-09.txt");
strcpy(nazwa_pliku_ami[9],"Jaw_2019-10.txt");
strcpy(nazwa_pliku_ami[10],"Jaw_2019-11.txt");
strcpy(nazwa_pliku_ami[11],"Jaw_2019-12.txt");

long dl_pl[MAXPL];


log=fopen ("ami-txt_log.txt","wt");
txt=fopen ("export-ami.txt","wt");

	
	cout<<"Program do konwersji ami->txt (C)DDB\n\n";
	fprintf(log,"Program do konwersji ami->txt (C)DDB\n\n");
	
	min_ts=0;
 
for(npl=0;npl<MAXPL;npl++)
	{
		cout<<"Analizowanie pliku ami cim ="<<nazwa_pliku_ami[npl]<<"\n";
	    ami=fopen (nazwa_pliku_ami[npl],"rb");
	
		if(ami==NULL)
		{
			cout<<"  B³¹d otwarcia pliku = "<<nazwa_pliku_ami[npl]<<"\n";
			return 0 ;
		}

		fprintf(log,"  Analiza pliku %s\n", nazwa_pliku_ami[npl]);
	
		dl_pl[npl]=dl_pliku_otw(ami);

		fprintf(log,"    Liczba linii w pliku = %i\n", dl_pl[npl] );
	
	    rewind (ami);
	    
	    min_ts=min_ts-8;
	    if(min_ts<0) min_ts=0;
	
		analiz_plik(ami, log, dl_pl[npl], min_ts);
	
	    fclose(ami);
	}


	cout<<"\n\n Wczytywanie plików\n";


	fprintf(log,"\n\n\n Liczba stacji %i\n", li_stacji);

	for(i=0;i<li_stacji;i++)
		{
			fprintf(log,"     |%s|\n", stacje[i]);
		}



	fprintf(log,"\n\n\n Liczba flaga _extr %i\n", li_f_e);

	for(i=0;i<li_f_e;i++)
		{
			fprintf(log,"     |%i|\n", f_e[i]);
		}


	fprintf(log,"\n\n\n Liczba kolumn %i\n", li_kolumn);

	for(i=0;i<li_kolumn;i++)
		{
			fprintf(log,"     |%s|\n", kolumny[i]);
		}



	fprintf(log,"\n\n\n Liczba timestamp %i\n", li_timestamp);

	for(i=0;i<li_timestamp;i++)
		{
			fprintf(log,"     |%s|\n", timestamp[i]);
		}



	fprintf(log,"\n\n\n wartoœæ max=%f\n", max_val);
	fprintf(log," wartoœæ min=%f\n", min_val);



    dane = new float[li_timestamp*(li_stacji+1)*li_kolumn];

for(i=0;i<li_timestamp*(li_stacji+1)*li_kolumn;i++)
{
	dane[i]=-1000000;
}


	min_ts=0;

for(npl=0;npl<MAXPL;npl++)
	{
		cout<<"Wczytywanie pliku ami cim ="<<nazwa_pliku_ami[npl]<<"\n";
	    ami=fopen (nazwa_pliku_ami[npl],"rb");
	
		if(ami==NULL)
		{
			cout<<"  B³¹d otwarcia pliku = "<<nazwa_pliku_ami[npl]<<"\n";
			return 0 ;
		}

		fprintf(log,"  Wczytywanie pliku %s\n", nazwa_pliku_ami[npl]);
	
	    rewind (ami);
	    
	    min_ts=min_ts-8;
	    if(min_ts<0) min_ts=0;
	
		wczytaj_plik(ami, log, dl_pl[npl], min_ts);

	    fclose(ami);
	}


	cout<<"\n\n Wypisywanie danych do plików\n";


	for(i=0;i<li_kolumn;i++)
		{
			fprintf(log,"     |%s|\n", kolumny[i]);
		}




	fprintf(txt,"TimeStamp;");

	for(i=0;i<li_stacji;i++)
		{
		for(j=0;j<li_kolumn;j++)
			{
				fprintf(txt,"%s=%s;", stacje[i],kolumny[j]);
			}
		}

	fprintf(txt,"\n");

	for(t=0;t<li_timestamp;t++)
		{
		fprintf(txt,"%s;",timestamp[t]	);
		for(s=0;s<li_stacji;s++)
			{
			for(k=0;k<li_kolumn;k++)
				{
					fprintf(txt,"%f;",dane[indeks(s, t, k)]);
				}
			}
		fprintf(txt,"\n");
		}

/*
for(i=0;i<li_timestamp*(li_stacji+1)*li_kolumn;i++)
{
	fprintf(txt,"%f\n",dane[i]);
}
*/

    delete [] dane;

    fclose(log);
    fclose(txt);
 
	cout<<"\n\n koniec - nacisnij ENTER aby zakonczyc \n";
	getchar();
	return 0;
}




int analiz_plik(FILE *ami, FILE *log, long dlpl, int &min_ts)
{
	
    long i,j,ii;
	char buf[MAXLINELENGTH], line[MAXLINELENGTH], line1[MAXLINELENGTH];
	char cpomo[MAXLINELENGTH];
	long fpoz;	
	int dl_st,li_dan_t, li_dan;
	long num_st=0, num_tim=0, num_kol=0;
	char fl;
	int pozyc,k;
	int ipomo;
	float fpomo;
	
	


     rewind(ami);
   	 fgets(line,MAXLINELENGTH-1,ami); // ominiêcie pierwszej linii pliku
     fprintf(log,"    linia nag³ówkowa -> %s",line);       
   	 
        for(i=0;i<dlpl;i++) // 
          {
            fgets(line,MAXLINELENGTH-1,ami);
            pozyc=0;
            k=0;

            while (pozyc!=-1)                               //wczytanie danych z jednego wiersza do macierzy
             {
              pozyc=getdata(line,line1,pozyc,';');
 
                if(strlen(line1)>=MAXRPD)
                  {
                      fprintf(log,"  -> B³¹d Zbyt d³uga dana nr %i o wartoœci %s w wierszu %i",k+1,line1,i+1);       
                      return -1;
                  }

                if(k==0)
                  {
                  	 fl=0;
                     for(ii=num_st;ii<li_stacji;ii++)
                     	{
                     		if(!strcmp(stacje[ii],line1))
                     		{
                     			fl=1;
                     			num_st=ii;
                     			break;
							}
						}

					 if(fl==0)
					 	{
	                     for(ii=0;ii<li_stacji;ii++)
	                     	{
	                     		if(!strcmp(stacje[ii],line1))
	                     		{
	                     			fl=1;
	                     			num_st=ii;
	                     			break;
								}
							}
						}

					 if(fl==0)
					 	{
							strcpy(stacje[li_stacji],line1);
					 		li_stacji++;
					 		if(li_stacji>=MAXRP)
					 		{
                      		fprintf(log,"  -> B³¹d zbyt du¿a liczba nazw stacji (za ma³a tablica) %i o wartoœci %s w wierszu %i",k+1,line1,i+1);       
                      		return -1;
							}
						}
                  }


                if(k==1)
                  {
                  	 strcpy(buf, line1);
                  }

                if(k==2)
                  {
//                  	 if(i<9000) 
//                  	 {	
                  	     sprintf(cpomo,"%s-%s",buf,line1);
                  	     
	                  	 fl=0;
	                     for(ii=num_tim;ii<li_timestamp;ii++)
	                     	{
	                     		if(!strcmp(timestamp[ii],cpomo))
	                     		{
	                     			num_tim=ii;
	                     			fl=1;
	                     			break;
								}
							}
						 
						 if(fl==0)	
						 	{
		                     for(ii=0;ii<li_timestamp;ii++)
		                     	{
		                     		if(!strcmp(timestamp[ii],cpomo))
		                     		{
		                     			num_tim=ii;
		                     			fl=1;
		                     			break;
									}
								}
							}

						 if(fl==0)
						 	{
								strncpy(timestamp[li_timestamp],cpomo,22);
						 		li_timestamp++;
						 		if(li_timestamp>=MAXDATETIME)
						 		{
	                      		fprintf(log,"  -> B³¹d zbyt du¿a liczba timestamp (za ma³a tablica) %i o wartoœci %s w wierszu %i",k+1,cpomo,i+1);       
	                      		return -1;
								}
							}
                  	
                  	
	                  	 fl=0;
	                     ipomo=atoi(line1); 
	                     for(ii=0;ii<li_f_e;ii++)
	                     	{
	                     		if(ipomo==f_e[ii])
	                     		{
	                     			fl=1;
								}
							}
						 if(fl==0)
						 	{
								f_e[li_f_e]=ipomo;
						 		li_f_e++;
						 		if(li_f_e>=MAXK)
						 		{
	                      		fprintf(log,"  -> B³¹d zbyt du¿a liczba nazw f_e(za ma³a tablica) %i o wartoœci %s w wierszu %i",k+1,line1,i+1);       
	                      		return -1;
								}
							}
//					 }
                  }

                if(k==3)
                  {
                  	 fl=0;
                     for(ii=0;ii<li_kolumn;ii++)
                     	{
                     		if(!strcmp(kolumny[ii],line1))
                     		{
                     			fl=1;
                     			break;
							}
						}
					 if(fl==0)
					 	{
							strcpy(kolumny[li_kolumn],line1);
					 		li_kolumn++;
					 		if(li_kolumn>=MAXK)
					 		{
                      		fprintf(log,"  -> B³¹d zbyt du¿a liczba nazw kolumn (za ma³a tablica) %i o wartoœci %s w wierszu %i",k+1,line1,i+1);       
                      		return -1;
							}
						}
                  }

                if(k==4)
                  {
                  	 zam_zn_dz(line1);
                     fpomo=atof(line1); 
                     if(fpomo<min_val)
                     {
                     	min_val=fpomo;
					 }

                     if(fpomo>max_val)
                     {
                     	max_val=fpomo;
					 }

                  }

              k++;    
             }
          }

/*
int li_stacji;
int li_kolumn;
int li_f_e;
char stacje[MAXRP][MAXRPD];
char kolumny[MAXK][MAXRPD];
int  f_e[MAXK];
float max_val, min_val;
*/

       fprintf(log,"\n");


min_ts=li_timestamp; // to zapewnia przeszukiwanie timestampów w zakresie jednego pliku a nie wszystkich
	
	
}





int wczytaj_plik(FILE *ami, FILE *log, long dlpl, int &min_ts)
{
	
    long i,j,ii;
	char buf[MAXLINELENGTH], line[MAXLINELENGTH], line1[MAXLINELENGTH];
	char cpomo[MAXLINELENGTH];
	long fpoz;	
	int dl_st,li_dan_t, li_dan;
	char fl;
	int pozyc,k;
	int ipomo;
	float fpomo;
	long num_st, num_tim, num_kol;
	int max_tim;
	
	
     rewind(ami);
   	 fgets(line,MAXLINELENGTH-1,ami); // ominiêcie pierwszej linii pliku
     //fprintf(log,"    linia nag³ówkowa -> %s",line);       
   	 
   	 num_st=0;
   	 num_tim=0;
   	 num_kol=0;
   	 
        for(i=0;i<dlpl;i++) // 
          {
            fgets(line,MAXLINELENGTH-1,ami);
            pozyc=0;
            k=0;

            while (pozyc!=-1)                               //wczytanie danych z jednego wiersza do macierzy
             {
              pozyc=getdata(line,line1,pozyc,';');
 
                if(strlen(line1)>=MAXRPD)
                  {
                      fprintf(log,"  -> B³¹d Zbyt d³uga dana nr %i o wartoœci %s w wierszu %i",k+1,line1,i+1);       
                      return -1;
                  }

                if(k==0)
                  {
                  	 fl=0;
                  	 
	                 for(ii=num_st;ii<li_stacji;ii++)
	                   	{
	                   		if(!strcmp(stacje[ii], line1))
	                   		{
	                   			fl=1;
	                   			num_st=ii;
	                   			break;
							}
						}
                  	 
					 if(fl==0)
					 	{
	                     for(ii=0;ii<li_stacji;ii++)
	                     	{
	                     		if(!strcmp(stacje[ii], line1))
	                     		{
	                     			fl=1;
	                     			num_st=ii;
	                     			break;
								}
							}
						}
						
					 if(fl==0)
					 	{
                      		fprintf(log,"  -> B³¹d - brak stacji %i o wartoœci %s w wierszu %i",k+1,line1,i+1);       
                      		return -1;
						}
                  }

                if(k==1)
                  {
                  	 strcpy(buf, line1);
                  }

                if(k==2)
                  {
                  	     sprintf(cpomo,"%s-%s",buf,line1);
                  	     
	                  	 fl=0;

	                     for(ii=num_tim;ii<li_timestamp;ii++)
	                     	{
	                     		if(!strcmp(timestamp[ii],cpomo))
	                     		{
	                     			fl=1;
	                     			num_tim=ii;
	                     			break;
								}
							}
	                  	 
	                  	 if(fl==0)
	                  	 {
		                     for(ii=0;ii<li_timestamp;ii++)
		                     	{
		                     		if(!strcmp(timestamp[ii],cpomo))
		                     		{
		                     			fl=1;
		                     			num_tim=ii;
		                     			break;
									}
								}
						}

						 if(fl==0)
						 	{
	                      		fprintf(log,"  -> B³¹d brak timestamp %i o wartoœci %s w wierszu %i",k+1,cpomo,i+1);       
	                      		return -1;
							}
                  }




                if(k==3)
                  {
                  	 fl=0;
                     for(ii=0;ii<li_kolumn;ii++)
                     	{
                     		if(!strcmp(kolumny[ii],line1))
                     		{	
                     			num_kol=ii;
                     			fl=1;
                     			break;
							}
						}
						
					 if(fl==0)
					 	{
                      		fprintf(log,"  -> B³¹d - brak nazwy kolumny %i o wartoœci %s w wierszu %i",k+1,line1,i+1);       
                      		return -1;
						}
                  }



                if(k==4)
                  {
                  	 zam_zn_dz(line1);
                     fpomo=atof(line1); 
                     
                     if( indeks(num_st, num_tim, num_kol)>li_timestamp*li_stacji*li_kolumn)
                     {
                     	fprintf(log," *** -> B³¹d - za du¿y indeks num_st=%i, num_tim=%i, num_kol=%i o dana %s w wierszu %i",num_st, num_tim, num_kol,line,i+1);    
					 }

                   	//fprintf(log," -> num_st=%i, num_tim=%i, num_kol=%i indeks=%i\n",num_st, num_tim, num_kol,indeks(num_st, num_tim, num_kol));    

                     
					 dane[indeks(num_st, num_tim, num_kol)]=fpomo;

                  }

              k++;    
             }
          }

/*
int li_stacji;
int li_kolumn;
int li_f_e;
char stacje[MAXRP][MAXRPD];
char kolumny[MAXK][MAXRPD];
int  f_e[MAXK];
float max_val, min_val;
*/

       fprintf(log,"\n");


min_ts=num_tim; // to zapewnia przeszukiwanie timestampów w zakresie jednego pliku a nie wszystkich
	
	
}






size_t indeks(long stac,long tim, long kol) // funkcja do zwracania wartoœci indeksu w du¿ej tablicy 3 wymiarowej
{
//	return (stac*li_timestamp*li_kolumn+kol*li_timestamp+kol);
return (tim*li_stacji*li_kolumn+stac*li_kolumn+kol);



//li_timestamp*li_stacji*li_kolumn
}


int getdata(char *line,char *dats,int poz, char separ)
//funkcja przeglada wiersz tekstu, rozpoczynajac od znaku 'poz',
//wydziela czesc do separatora jako jedna zmienna wskazywana przez dats,
//zwraca poz rowna napotkanemu separatorowi,
//gdy odczyta znak konca wiersza EOL to poz=-1 - oznacza, ze wszystkie dane w wierszu zostaly wydzielone
{
    int i=0;
    int j=0;

 
     while ((line[i+poz]!=separ)&&(line[i+poz]!='\n')&&(line[i+poz]!='\r')&&(line[i+poz]!=0))
       {
         i++;
       }

     for(j=0;j<i;j++) dats[j]=line[poz+j];

     dats[i]='\0';

     i++;

     poz=poz+i;
     
     if ((line[poz]=='\n')||(line[poz]=='\r')||(line[poz]==0)) poz=-1;
        
    return(poz);
}




long dl_pliku(char *nazwa) // podaje liczbe wierszy w pliku
{
FILE *fp;
long dp;
char li[MAXLINELENGTH];


dp=0;

  if((fp=fopen(nazwa,"rt"))==NULL)
    {
      return(-1);  
    }
  else
    {
      while (fgets(li,MAXLINELENGTH-1,fp)!=NULL)
        {
            dp++;
        }
    }

  fclose(fp);

  return (dp);
}



long dl_pliku_otw(FILE *fp) // podaje liczbe wierszy w pliku
{
  long dp;
  char li[MAXLINELENGTH];

  rewind (fp);
  dp=0;

      while (fgets(li,MAXLINELENGTH-1,fp)!=NULL)
        {
            dp++;
        }

  rewind(fp);
 return (dp);
}

void zam_zn_dz(char *tekst)
{
int i,j;

j=strlen(tekst);
 for(i=0;i<j;i++)
  {
   if(tekst[i]==',') tekst[i]='.';
  }
}


void zam_spac_sr(char *tekst, char *buf)
{
int i,j;
int bi;
int fspac=0;
int len;

bi=0;

len=strlen(tekst);
 for(i=0;i<len;i++)
  {
  	if(tekst[i]!=' ') 
	  {
	   buf[bi]=tekst[i];
	   fspac=0;
	   bi++;
      }
  	
    if(tekst[i]==' '&&fspac==0)
	  {
	   buf[bi]=';';
	   bi++;
	   fspac=1;
      }
  }
 
 buf[bi]=0;
  
}



/*
void zam_zn_dz_CS(CString *tekst)
{
int i,j;

j=tekst->GetLength();
 for(i=0;i<j;i++)
  {
   if(tekst->GetAt(i)==',') tekst->SetAt(i,'.');
  }
}
*/

