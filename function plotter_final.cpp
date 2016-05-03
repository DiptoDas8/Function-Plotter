#include "iGraphics.h"
#include <string.h>
#include <stdlib.h>
#include <math.h>
#define window_sizeX 1150
#define window_sizeY 700
//Drawing functions
void initial_Window(void);
void cartesian_draw();
void polar_draw();


void help_view(void);
void show_credits(void);
//void polar_draw();

void save_points(int func_save);
void load_function(int func_load);


int showpointflag=0;
int mode=0;
int paramode=1;
int errorflag=0;
double alpha,beta;
int primlength=0;
int calcuflag=0;
int graph_to_show=1;
int current=1;
double increment;
int zoomflag=2;
double leftrange=-30.0;
double rightrange=30.0;
double zoom=10.0;
int totalarray=600;
int newy1=400,newy2=432;
int calculated=0;
int axis[]={30,20,10,0,-10,-20,-30};
int polar[]={0,6,12,18,24,30};
double axisvalue=1.0;
int func_save;
int func_load=0;

double xvalue[8][2450];
double yvalue[8][2450];
double finalx[8][2450];
double finaly[8][2450];
int mapvalue [8][2450];

char primary[8][1000];
char secondary[1000];
char stringnum[1000];
char strng[1000];
int operat[1000];
double number[1000];
double numbers[1000];
int pointcheck(double p1,double p2,double p3,double p4){
	if(p1>=50 && p1<=650 && p3>=50 && p3<=650 && p2>=83 && p2<=685 && p4>=83 && p4<=685){
		return 1;
	}
	return 0;
}
void left_shift_sec(int i, int n){
    int j=1;
    int k;
    while(j<=n){
        for(k=i;secondary[k]!='\0';++k){
            secondary[k-1]=secondary[k];
        }
        secondary[k-1]='\0';
        i--;
        j++;
    }
}
void right_shift_sec(int i, int n){
    int j=1;
    int k;
    int l;
    while(j<=n){
        l=strlen(secondary);
        for(k=l-1;k>=i;--k){
            secondary[k+1]=secondary[k];
        }
        secondary[l+1]='\0';
        i++;
        j++;
    }
}


char defops[]={'b','+','-','*','/','^','s','c','t','l','e','a'};
int searchop(int i){
    int flag=0;
    int j;
    for(j=1;j<=11;++j){
        if(strng[i]==defops[j]){
            flag=1;
            break;
        }
    }
    if(flag){
        return j;
    }
    else{
        return 0;
    }
}

void left_shift_array(int i, int n){
    int j=1;
    int k;
    while(j<=n){
        for(k=i;operat[k]!=-1;++k){
            operat[k-1]=operat[k];
            number[k-1]=number[k];
        }
        operat[k-1]=-1;
        i--;
        j++;
    }
}
double pwr(double n,int p){
    double s=1.0;
    int i=1;
    while(i<=p){
        s=s*n;
        i++;
    }
    return s;
}

long int p10(int p){
    long int s=1;
    int i=1;
    while(i<=p){
        s=s*10;
        i=i+1;
    }
    return s;
}
int lenn(long int n){
    int l;
    l=ceil(log10(n));
	long int pp;
    pp=p10(l);
    if(pp==n){
        l=l+1;
    }
    return l;
}
double returny(double x){
    double pi=acos(-1.0);
    int i;
    int c;
    operat[0]=0;
    int j=1;
    if(strng[0]!='-'){
        operat[j]=1;
        j++;
    }
    int lens=strlen(strng);
    for(i=0;i<lens;++i){
        if(c=searchop(i)){
            operat[j]=c;
            if(operat[j]==11){
                if(strng[i+1]=='c'){
                    operat[j]+=1;
                }
                else if(strng[i+1]=='t'){
                    operat[j]+=2;
                }
            }
            if(c==11){
                i+=3;
            }
            else if(c==9){
                if(strng[i+1]=='n'){
                    i++;
                    operat[j]=14;
                }
                else{
                    i+=2;
                }
            }
            else if(c>=6 && c<=8){
                i+=2;
            }
            j++;
        }
        else{
            operat[j]=0;
            j++;
            while(!searchop(i+1)){
                i++;
            }
        }
    }
    operat[j]=-1;
    int numi=1;
    int minusflag=0;
    double a;
    int flag=0;
    int decten;
    a=0.0;
    int numfoundflag=0;
    double bb,dd;
    double temp;
    for(i=0;i<lens;++i){
        if(strng[i]=='_'){
            minusflag=1;
        }
        else if((mode==1 && strng[i]=='x') || (mode==2 && strng[i]=='D') || (mode==3 && strng[i]=='t')){
            numbers[numi]=x;
            numi++;
            a=0.0;
            numfoundflag=0;
            flag=0;
        }
        else if(strng[i]=='.'){
            flag=1;
            decten=1;
        }
        else if(strng[i]>=48 && strng[i]<=57){
            if(!flag){
                a=(a*10.0)+(strng[i]-48)*1.0;
            }
            else{
                bb=pwr(10.0,decten);
                dd=1/(bb);
                a=a+(dd*(strng[i]-48));
                decten++;
            }
            numfoundflag=1;
        }
        else{
            if(numfoundflag){
            if(minusflag){
                a=a*(-1);
            }
            numbers[numi]=a;
            minusflag=0;
            flag=0;
            a=0.0;
            numi++;
            numfoundflag=0;
            }

        }

    }
    if(numfoundflag){
            if(minusflag)a=a*(-1);
            numbers[numi]=a;
            flag=0;
            a=0;
            numfoundflag=0;
        }

    number[0]=0.00;
    int k=1;
    for(i=1;operat[i]!=-1;++i){
        if(operat[i]){
            number[i]=0.00;
        }
        else{
            number[i]=numbers[k];
            k++;
        }
    }

    for(i=1;operat[i]!=-1;++i){
        if(operat[i]>=6 && operat[i]<=14){
            if(operat[i]==13){
                temp=atan(number[i+1]);
            }
            else if(operat[i]==12){
				if(number[i+1]>1.0 || number[i+1]<-1.0){
					temp=-23;
					errorflag=1;
				}
				else{
					temp=acos(number[i+1]);
				}
            }
            else if(operat[i]==11){
                if(number[i+1]>1.0 || number[i+1]<-1.0){
					temp=-23;
					errorflag=1;
				}
				else{
					temp=asin(number[i+1]);
				}
            }
            else if(operat[i]==10){
                temp=exp(number[i+1]);
            }
            else if(operat[i]==14){
				if(number[i+1]<0.0){
					temp=-23;
					errorflag=1;
				}
				else{
					temp=log(number[i+1]);
				}
                
            }
            else if(operat[i]==9){
				if(number[i+1]<0.0){
					temp=-23;
					errorflag=1;
				}
				else{
					temp=log10(number[i+1]);
				}
            }
            else if(operat[i]==8){
                temp=tan(number[i+1]);
            }
            else if(operat[i]==7){
                temp=cos(number[i+1]);

            }
            else if(operat[i]==6){
                temp=sin(number[i+1]);

            }
            left_shift_array(i+1,1);
            number[i]=temp;
            operat[i]=0;
            i=0;

        }
    }
    for(i=1;operat[i]!=-1;++i){
        if(operat[i]==5){
            temp=pow(number[i-1],number[i+1]);
            left_shift_array(i+1,2);
            operat[i-1]=0;
            number[i-1]=temp;

            i=0;
        }

    }
    for(i=1;operat[i]!=-1;++i){
        if(operat[i]==4){
			if(number[i+1]==0){
				temp=-23;
				errorflag=1;
			}
			else{
				temp=(number[i-1])/(number[i+1]);
			}
            left_shift_array(i+1,2);
            operat[i-1]=0;
            number[i-1]=temp;

            i=0;
        }

    }
    for(i=1;operat[i]!=-1;++i){
        if(operat[i]==3){
            temp=(number[i-1])*(number[i+1]);
            left_shift_array(i+1,2);
            operat[i-1]=0;
            number[i-1]=temp;

            i=0;
        }

    }
    for(i=1;operat[i]!=-1;++i){
        if(operat[i]==1 || operat[i]==2){
        if(operat[i]==2){
            temp=(number[i-1])-(number[i+1]);
        }
        else if(operat[i]==1){
            temp=(number[i-1]+number[i+1]);
        }
            left_shift_array(i+1,2);
            operat[i-1]=0;
            number[i-1]=temp;

            i=0;
        }

    }

    return number[0];
}
int int_to_string(double x){
    int i=0;
    int lenx;
    int j;
    double floatx;
    long int intx;
    if(x<0){
        stringnum[i]='_';
        i++;
        x=x*(-1.0);
    }
    intx=x;
    floatx=x-(double)intx;
    if(intx==0){
        lenx=1;
    }
    else{
        lenx=lenn(intx);
    }

    for(j=(i+lenx-1);j>=i;--j){
        stringnum[j]=(intx%10)+48;
        intx=intx/10;
    }
    i+=lenx;
    stringnum[i]='.';
    i++;
    for(j=1;j<=3;++j){
        floatx=floatx*10.0;
        stringnum[i]=(((int)floatx)%10)+48;
        i++;
    }
    stringnum[i]='\0';
    return i;
}
void calculate(void){
    int prim_length,i,j,fin_length,init_length,dif;
    double ret_value;
    double x,y;
	int arrano=1;
	int secondarylen;
    int firstbracket,secondbracket;
	int func_serial;
	for(func_serial=1;func_serial<=current;++func_serial){
    prim_length=strlen(primary[func_serial]);
	arrano=1;
    for(x=leftrange;x<=rightrange;x=x+increment){
    for(i=0;i<prim_length;++i){
        secondary[i]=primary[func_serial][i];
    }
    secondary[i]='\0';
	secondarylen=i;
    for(i=0;i<secondarylen;++i){
        if(secondary[i]=='('){
            firstbracket=i;
            for(j=i;j<strlen(secondary);++j){
                if(secondary[j]=='('){
                    firstbracket=j;
                }
                else if(secondary[j]==')'){
                    secondbracket=j;
                    break;
                }
            }
            init_length=(secondbracket-firstbracket)+1;
            int k=0;
            for(j=firstbracket+1;j<secondbracket;++j){
                strng[k]=secondary[j];
                k++;
            }
            strng[k]='\0';
            ret_value=returny(x);
            fin_length=int_to_string(ret_value);
            dif=fin_length-init_length;
            if(dif<0){
                dif=dif*(-1);
                left_shift_sec(secondbracket+1,dif);
            }
            else{
                right_shift_sec(secondbracket+1,dif);
            }
            for(j=0;j<fin_length;++j){
                secondary[firstbracket]=stringnum[j];
                firstbracket++;

            }
            i=-1;
        }
    }
    for(i=0;i<strlen(secondary);++i){
        strng[i]=secondary[i];
    }
    strng[i]='\0';
    y=returny(x);
	if(mode==1){
		xvalue[func_serial][arrano]=x;
		yvalue[func_serial][arrano]=y;
	}
	else if(mode==2){
		xvalue[func_serial][arrano]=y*(cos(x));
		yvalue[func_serial][arrano]=y*(sin(x));
	}
	else if(mode==3){
		if(func_serial%2){
			xvalue[func_serial][arrano]=y;
		}
		else{
			yvalue[func_serial-1][arrano]=y;
		}
	}
	mapvalue[func_serial][arrano]=errorflag;
	errorflag=0;
	arrano++;
    }
	}
}

/*
	Function iDraw() is called again and again by the system
*/

void iDraw()
{
	//place your codes here
	iClear();
	initial_Window();
}



/*
	Function iMouseMove() is called mouse is dragged while pressed.
	(mx, my) is the position where the mouse pointer is
*/
void iMouseMove(int mx, int my)
{
	printf("%d %d\n",mx,my);
	//place your codes here
}



/*
	Function iMouse() is called when mouse is pressed/released.
	(mx, my) is the position where the mouse pointer is
*/
void iMouse(int button, int state, int mx, int my)
{
	if(button == GLUT_LEFT_BUTTON && state == GLUT_DOWN)
	{
		//place your codes here
		if(mode==0){
			if((mx>=400 && mx<=464) && (my>=200&&my<=264))
				mode=4;
			else if((mx>=250 && mx<=378) && (my>=200 && my<=328))
				mode=5;

			else if((mx>=700 && mx<=844)&&(my>=400 && my<= 440)){
				mode=1;
				primlength=0;
				current=1;
				primary[1][0]='\0';
				primary[2][0]='\0';
				primary[3][0]='\0';
				calculated=0;
				calcuflag=0;
				newy1=400;
				newy2=432;
				zoomflag=2;
				zoom=10.0;
				axisvalue=1.0;
				totalarray=600;
				leftrange=-30.0;
				rightrange=30.0;
				increment=0.1;
			}
			else if((mx>=700 && mx<=844)&&(my>=300 && my<= 340)){
				mode=2;
				primlength=0;
				current=1;
				primary[1][0]='\0';
				primary[2][0]='\0';
				primary[3][0]='\0';
				calculated=0;
				calcuflag=0;
				newy1=400;
				newy2=432;
				zoomflag=2;
				zoom=10.0;
				axisvalue=1.0;
				totalarray=628;
				leftrange=0.00;
				rightrange=6.28;
				increment=0.01;
			}
		}
		else if(mode==1){
			if((mx>=1040 && mx<=1072)&&(my>=newy1 && my<=newy2)){
				if(current<3 && primlength){
					current++;
					newy1=newy1-100;
					newy2=newy2-100;
					calcuflag=0;
					primlength=0;
				}
			}
			else if(mx>=1080 && mx<=1112 && my>=newy1 && my<=newy2){
				if(current>1){
					current--;
					newy1+=100;
					newy2=newy2+100;
					calcuflag=0;
					primlength=0;
					calculated=current;
				}
			}
			else if(mx>=823 && mx<=823+32 && my>=609 && my<=609+32){
				if(zoomflag<4){
					zoomflag++;
					zoom=zoom*2;
					leftrange=leftrange/2;
					rightrange=rightrange/2;
					totalarray=totalarray/2;
					axisvalue=axisvalue/2;
				}
			}
			else if(mx>=873 && mx<=873+32 && my>=609 && my<=609+32){
				if(zoomflag>0){
					zoomflag--;
					zoom=zoom/2;
					leftrange=leftrange*2;
					rightrange=rightrange*2;
					totalarray=totalarray*2;
					axisvalue=axisvalue*2;
				}
			}
			else if(mx>=1000 && mx<=1064 && my>=100 && my<=164){
				mode=0;
			}
			else if(mx>=900 && mx<=964 && my>=100 && my<=164){
				primlength=0;
				current=1;
				primary[1][0]='\0';
				primary[2][0]='\0';
				primary[3][0]='\0';
				calculated=0;
				calcuflag=0;
				newy1=400;
				newy2=432;
				zoomflag=2;
				zoom=10.0;
				axisvalue=1.0;
				totalarray=600;
				leftrange=-30.0;
				rightrange=30.0;
			}
		}
		else if(mode==2){
			if((mx>=1040 && mx<=1072)&&(my>=newy1 && my<=newy2)){
				if(current<3 && primlength){
					current++;
					newy1=newy1-100;
					newy2=newy2-100;
					calcuflag=0;
					primlength=0;
				}
			}
			else if(mx>=1080 && mx<=1112 && my>=newy1 && my<=newy2){
				if(current>1){
					current--;
					newy1+=100;
					newy2=newy2+100;
					calcuflag=0;
					primlength=0;
					calculated=current;
				}
			}
			else if(mx>=823 && mx<=823+32 && my>=609 && my<=609+32){
				if(zoomflag<4){
					zoomflag++;
					zoom=zoom*2;
					axisvalue=axisvalue/2;
				}
			}
			else if(mx>=873 && mx<=873+32 && my>=609 && my<=609+32){
				if(zoomflag>0){
					zoomflag--;
					zoom=zoom/2;
					axisvalue=axisvalue*2;
				}
			}
			else if(mx>=1000 && mx<=1064 && my>=100 && my<=164){
				mode=0;
			}
			else if(mx>=900 && mx<=964 && my>=100 && my<=164){
				primlength=0;
				current=1;
				primary[1][0]='\0';
				primary[2][0]='\0';
				primary[3][0]='\0';
				calculated=0;
				calcuflag=0;
				newy1=400;
				newy2=432;
				zoomflag=2;
				zoom=10.0;
				axisvalue=1.0;
				totalarray=628;
				leftrange=0.0;
				rightrange=6.28;
			}
		}
		else if(mode==4){
			if(mx>=985 && mx<=985+64 && my>=100 && my<=164)mode=0;
		}
		else if(mode==5){
			if(mx>=985 && mx<=985+64 && my>=100 && my<=164)mode=0;
		}
		if(mx>=1120 &&  mx<=1152)
			{
				if(my>=400 && my<=432)
					func_save=1;
				else if(my>=300 && my<=332)
					func_save=2;
				else if(my>=200 && my<=232)
					func_save=3;
			}
		if(mx>=1080 && mx<=1112)
		{
			if(my>=365 && my<=397)
				func_load=1;
			else if(my>=265 && my<=297)
				func_load=2;
			else if(my>=165 && my<=197)
				func_load=3;
		}
		
	}
	if(button == GLUT_RIGHT_BUTTON && state == GLUT_DOWN)
	{
		//place your codes here
	}
}



/*
	Function iKeyboard() is called whenever a ey is hit.
	key holds the ASCII value of the key pressed.
	*/
void iKeyboard(unsigned char key)
{
	if(mode==1 || mode==2){
		if(key=='\r' && primlength){
			calcuflag=1;
		}
		if(!calcuflag){
			if(key==127 || key==8){
				if(primlength>0){
					primary[current][primlength-1]='\0';
					primlength--;
				}
			}
			else{
				primary[current][primlength]=key;
				primary[current][primlength+1]='\0';
				primlength++;
			}
		}
	}

	//place your codes here
}



/*
	function iSpecialKeyboard() is called whenver user hits special keys like-
	function keys, home, end, pg up, pg down, arraows etc. you have to use
	appropriate constants to detect them. A list is:
	GLUT_KEY_F1, GLUT_KEY_F2, GLUT_KEY_F3, GLUT_KEY_F4, GLUT_KEY_F5, GLUT_KEY_F6,
	GLUT_KEY_F7, GLUT_KEY_F8, GLUT_KEY_F9, GLUT_KEY_F10, GLUT_KEY_F11, GLUT_KEY_F12,
	GLUT_KEY_LEFT, GLUT_KEY_UP, GLUT_KEY_RIGHT, GLUT_KEY_DOWN, GLUT_KEY_PAGE UP,
	GLUT_KEY_PAGE DOWN, GLUT_KEY_HOME, GLUT_KEY_END, GLUT_KEY_INSERT
	*/
void iSpecialKeyboard(unsigned char key)
{
	if (key == GLUT_KEY_END)
	{
		exit(0);
	}
}



int main()
{
	//place your initialization codes here.
	iInitialize(window_sizeX,window_sizeY,"Function Plotter");
	return 0;
}



void initial_Window(void)
{
	//creates the window to show
	if(mode==0){
		iSetColor(0,255,100);
		iFilledRectangle(0,0,window_sizeX,window_sizeY);
		iShowBMP(0,0,"func_plot.bmp");
		iShowBMP(700,400,"cartesian.bmp");
		iShowBMP(700,300,"polar.bmp");
		
		iShowBMP(400,200,"help.bmp");
		iShowBMP(250,200,"credit.bmp");
	}
	
	if(mode==1) cartesian_draw();
	else if(mode==2) polar_draw();

	else if(mode==4) help_view();
	else if(mode==5) show_credits();


}

void cartesian_draw()
{
	int i,mx,my,arrayi,count,fxpos,func_serial,lekhay=405;
	double p1,p2,p3,p4;
	int axisi=1;
	int yaxisy,xaxisx,temp2;
	char buffer[10];
	iSetColor(0,0,255);

	iShowBMP(670,0,"rene_descartes.bmp");
	iSetColor(0,0,0);
	iFilledRectangle(0,0,670,700);
	iSetColor(100,100,100);
	iFilledRectangle(51,window_sizeY-620,600,600);
	iSetColor(255,255,255);
	iFilledRectangle(54,window_sizeY-617,594,594);
	iShowBMP(730,450,"func_input.bmp");
	iShowBMP(1000,100,"goback.bmp");
	iShowBMP(900,100,"reset.bmp");
	if(zoomflag<4){
		iShowBMP(823,609,"zoom_in.bmp");
	}
	if(zoomflag>0){
		iShowBMP(873,609,"zoom_out.bmp");
	}
	fxpos=399;
	if(current<3){
		iShowBMP(1040,newy1,"new.bmp");
	}
	if(current>1){
		iShowBMP(1080,newy1,"delete.bmp");
	}
	if(current<3||current>1){
		iShowBMP(1120,newy1,"save.bmp");
		iShowBMP(1080,newy1-35,"load.bmp");
	}
	for(func_serial=1;func_serial<=current;++func_serial)
		{
			iShowBMP(700,fxpos,"y=f(x).bmp");
			iSetColor(204,255,153);
			iFilledRectangle(735,fxpos+1,300,30);
			fxpos-=100;
		}
	iSetColor(230,230,250);
	count=0;
	for(i=-3;(594-i)>0;i=i+10)
	{
		if(count%10==0) iSetColor(105,105,105);
		iRectangle(54+i,window_sizeY-617,594-i,594);
		count++;
		iSetColor(230,230,250);
	}
	iSetColor(230,230,250);
	count=0;
	for(i=-3;(594-i)>0;i=i+10)
	{
		if(count%10==0) iSetColor(205,133,63);
		iRectangle(54,window_sizeY-617+i,594,594-i);
		count++;
		iSetColor(230,230,250);
	}
	iSetColor(72,61,139);
	mx=(54+594+55)/2;
	my=(window_sizeY-617+594+82)/2;
	alpha = mx;
	beta = my;
	iFilledRectangle(mx,window_sizeY-617,2,594);
	iFilledRectangle(54,my,594,2);
	iSetColor(255,255,255);
	iFilledRectangle(50,window_sizeY-640,601,20);
	iFilledRectangle(12,window_sizeY-618,38,600);
	iSetColor(0,0,0);
	iRectangle(50,window_sizeY-640,601,22);
	iRectangle(12,window_sizeY-618,38,600);
	for(func_serial=1;func_serial<=current;++func_serial){
		iText(750,lekhay,primary[func_serial],GLUT_BITMAP_TIMES_ROMAN_24);
		lekhay=lekhay-100;
	}
	yaxisy=673;
	for(axisi=0;axisi<7;++axisi){
		temp2=axis[axisi]*axisvalue;
		itoa(temp2,buffer,10);
		
		if(zoomflag==4 && (axisi==0 || axisi==6 || axisi==2 || axisi==4)){
			if(axisi==0){
				iText(18,yaxisy,"7.5");
			}
			else if(axisi==6){
				iText(18,yaxisy,"-7.5");
			}
			else if(axisi==4){
				iText(18,yaxisy,"-2.5");
			}
			else if(axisi==2){
				iText(18,yaxisy,"2.5");
			}
		}
		else{
			iText(18,yaxisy,buffer);
		}
		yaxisy=yaxisy-98;

	}
	xaxisx=53;
	for(axisi=6;axisi>=0;--axisi){
		temp2=axis[axisi]*axisvalue;
		itoa(temp2,buffer,10);
		
		if(zoomflag==4 && (axisi==0 || axisi==6 || axisi==2 || axisi==4)){
			if(axisi==0){
				iText(xaxisx,70,"7.5");
			}
			else if(axisi==6){
				iText(xaxisx,70,"-7.5");
			}
			else if(axisi==4){
				iText(xaxisx,70,"-2.5");
			}
			else if(axisi==2){
				iText(xaxisx,70,"2.5");
			}
		}
		else{
			iText(xaxisx,70,buffer);
		}
		xaxisx=xaxisx+98;

	}
	iText(54,360,"X' ",GLUT_BITMAP_TIMES_ROMAN_24);
	iText(625,360,"X ",GLUT_BITMAP_TIMES_ROMAN_24);
	iText(330,360,"O ",GLUT_BITMAP_TIMES_ROMAN_24);
	iText(330,656,"Y ",GLUT_BITMAP_TIMES_ROMAN_24);
	iText(328,85,"Y' ",GLUT_BITMAP_TIMES_ROMAN_24);


	if(calcuflag){
		calculate();
		calculated=current;
	}
		for(func_serial=1;func_serial<=calculated;++func_serial){
		for(arrayi=1;arrayi<=totalarray;++arrayi){
			finalx[func_serial][arrayi]=xvalue[func_serial][arrayi]*(zoom)+alpha;
			finaly[func_serial][arrayi]=yvalue[func_serial][arrayi]*(zoom)+beta;
		}
		for(arrayi=1;arrayi<totalarray;++arrayi){
			p1=finalx[func_serial][arrayi];
			p2=finaly[func_serial][arrayi];
			p3=finalx[func_serial][arrayi+1];
			p4=finaly[func_serial][arrayi+1];
			if(mapvalue[func_serial][arrayi]==0 && mapvalue[func_serial][arrayi+1]==0 && pointcheck(p1,p2,p3,p4)){
				if(func_serial==1) iSetColor(0,100,0);
				else if(func_serial==2) iSetColor(255,0,0);
				else if(func_serial==3) iSetColor(25,25,112);
				iLine(p1,p2,p3,p4);
			}
		}

		}
			
	if(func_save==1||func_save==2||func_save==3)
		//iText(300,200,primary[func_save]);
		save_points(func_save);
	if(func_load==1||func_load==2||func_load==3){
		load_function(func_load);
		calcuflag=1;
		calculated=func_load;
		func_load=0;
		primlength=1;
	}


}

void polar_draw()
{
	int i,fxpos,mx,my,func_serial,arrayi;
	int lekhay=405;
	int axisi=1;
	int xaxisx,temp2;
	char buffer[10];
	double p1,p2,p3,p4;
	iSetColor(0,0,255);
	iShowBMP(0,0,"hipparchus.bmp");
	iSetColor(0,0,0);
	iFilledRectangle(0,0,670,700);
	iSetColor(100,100,100);
	iFilledRectangle(51,window_sizeY-620,600,600);
	iSetColor(255,255,255);
	iFilledRectangle(54,window_sizeY-617,594,594);
	iSetColor(0,0,255);
	iSetColor(0,0,0);
	iSetColor(112,128,144);
	mx=(54+594+53)/2;
	my=(window_sizeY-617+594+81)/2;
	alpha = mx;
	beta = my;
	iFilledRectangle(alpha,window_sizeY-617,2,594);
	iFilledRectangle(54,beta,594,2);
	iSetColor(176,196,222);
	for(i=0;i<=540;i=i+60)
	{
		if(i<=300)
			iCircle(alpha,beta,i);
	}
	iShowBMP(730,450,"func_input.bmp");
	iShowBMP(1000,100,"goback.bmp");
	iShowBMP(900,100,"reset.bmp");
	if(zoomflag<4){
		iShowBMP(823,609,"zoom_in.bmp");
	}
	if(zoomflag>0){
		iShowBMP(873,609,"zoom_out.bmp");
	}
	fxpos=399;
	if(current<3){
		iShowBMP(1040,newy1,"new.bmp");
	}
	if(current>1){
		iShowBMP(1080,newy1,"delete.bmp");
	}
	if(current<3||current>1){
		iShowBMP(1120,newy1,"save.bmp");
		iShowBMP(1080,newy1-35,"load.bmp");
	}
	for(func_serial=1;func_serial<=current;++func_serial)
		{
			iShowBMP(700,fxpos,"r=f(D).bmp");
			iFilledRectangle(735,fxpos+1,300,30);
			fxpos-=100;
		}
	iSetColor(0,0,0);
	for(func_serial=1;func_serial<=current;++func_serial){
		iText(750,lekhay,primary[func_serial],GLUT_BITMAP_TIMES_ROMAN_24);
		lekhay=lekhay-100;
	}
	xaxisx=alpha-10;
	for(axisi=0;axisi<6;++axisi){
		temp2=polar[axisi]*axisvalue;
		itoa(temp2,buffer,10);
		
		if(zoomflag==4 && (axisi==1 || axisi==3 || axisi==5)){
			if(axisi==1)iText(xaxisx,beta-8,"1.5");
			else if(axisi==3)iText(xaxisx,beta-8,"4.5");
			else if(axisi==5)iText(xaxisx,beta-8,"7.5");
		}
		else{
			iText(xaxisx,beta-8,buffer);
		}
		xaxisx=xaxisx+56;

	}
	if(calcuflag){
		calculate();
		calculated=current;
	}
		for(func_serial=1;func_serial<=calculated;++func_serial){
		for(arrayi=1;arrayi<=totalarray;++arrayi){
			finalx[func_serial][arrayi]=xvalue[func_serial][arrayi]*(zoom)+alpha;
			finaly[func_serial][arrayi]=yvalue[func_serial][arrayi]*(zoom)+beta;
		}
		for(arrayi=1;arrayi<totalarray;++arrayi){
			p1=finalx[func_serial][arrayi];
			p2=finaly[func_serial][arrayi];
			p3=finalx[func_serial][arrayi+1];
			p4=finaly[func_serial][arrayi+1];
			if(mapvalue[func_serial][arrayi]==0 && mapvalue[func_serial][arrayi+1]==0 && pointcheck(p1,p2,p3,p4)){
				if(func_serial==1) iSetColor(0,100,0);
				else if(func_serial==2) iSetColor(255,0,0);
				else if(func_serial==3) iSetColor(25,25,112);
				iLine(p1,p2,p3,p4);
			}
			

		}

		}

		if(func_save==1||func_save==2||func_save==3)
		save_points(func_save);
	if(func_load==1||func_load==2||func_load==3){
		load_function(func_load);
		calcuflag=1;
		calculated=func_load;
		func_load=0;
		primlength=1;
	}

}



void save_points(int func_save)
{
	FILE *fp;
    char x[10],y[10];
    int i;
	
	//iText(300,200,file_name);

    if((fp=fopen("Points.txt","w"))==NULL)
		iText(300,300,"kicchu na");
	else
	{
		fprintf(fp,"Graph of %s:\n\n",primary[func_save]);
		fprintf(fp,"%10s %10s\n"," X "," Y ");
		for(i=1;i<=totalarray;i++)
		{
			sprintf(x,"%3f",xvalue[func_save][i]);
			sprintf(y,"%3f",yvalue[func_save][i]);
			fprintf(fp,"%10s %10s\n",x,y);
		}
		fprintf(fp,"%s","\n************************************\n\n");
		fclose(fp);
	}
}

void help_view(void)
{
	iShowBMP(100,100,"help_window.bmp");
	iShowBMP(985,100,"goback.bmp");


}
void load_function(int func_load)
{
	char load_file_name[15];
	char str[80];
    FILE *fp;

	if(mode==1 && func_load==1) strcpy(load_file_name,"cart_load1.txt");
	else if(mode==1 && func_load==2) strcpy(load_file_name,"cart_load2.txt");
	else if(mode==1 && func_load==3) strcpy(load_file_name,"cart_load3.txt");
	else if(mode==2 && func_load==1) strcpy(load_file_name,"pol_load1.txt");
	else if(mode==2 && func_load==2) strcpy(load_file_name,"pol_load2.txt");
	else if(mode==2 && func_load==3) strcpy(load_file_name,"pol_load3.txt");

	if((fp=fopen(load_file_name,"r"))!=NULL)
	{
		fscanf(fp,"%s",str);
		strcpy(primary[func_load],str);
	}

}

void show_credits(void)
{
	iShowBMP(100,100,"credits.bmp");
	iShowBMP(985,100,"goback.bmp");
}