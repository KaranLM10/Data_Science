#!/usr/bin/env python
# coding: utf-8

# In[33]:


#importing library
import pandas as pd  
import numpy as np  
import scipy
from scipy.stats.stats import pearsonr
from pylab import rcParams
import matplotlib.pyplot as plt  
from sklearn.model_selection import train_test_split 
from sklearn.linear_model import LinearRegression
from sklearn import metrics
import statsmodels.api as sm
from statsmodels.formula.api import ols
from statsmodels.sandbox.regression.predstd import wls_prediction_std
import matplotlib.pyplot as plt
import seaborn as sns


# In[34]:


path = 'C:/Users/91993/Documents/Python/Assignment1/movie_metadata.csv'
df = pd.read_csv(path,delimiter=",")
#reading file


# In[37]:


year=df['title_year'].unique()
print(year)
df_new = df
df_new


# In[38]:


df_new

df_new['genres']=df_new['genres'].str.lower()
#Convert Genre to lower case
df_new['movie_title']=df_new['movie_title'].str.lower().map(lambda x : x.strip())
df_dict_act = df_new.sort_values(by='gross', ascending=False)
#sorting data frame by descending
dir_c=df_new['director_name'].nunique()
print(dir_c)

df_new['movie_title']=df_new['movie_title'].drop_duplicates()
df_dict_act
#df_new


# In[27]:


df_dict_act['movie_title']


# In[39]:



###############################################################################################################################

#Error Checking & Validation Function

###############################################################################################################################

#Creating function that will prompt user to enter Numeric Values only.
def getInput(prompt):
    value = input(prompt)
    while not value.isnumeric():
        #Restraining user to enter numeric values wherever needed.
        print("\nThe Value should be numeric here")
        value = input(prompt)
    return int(value)

#Creating function that will prompt user to enter String Values only.
def getstrInput(prompt):
    value = input(prompt)
    while value.isnumeric():
        #Restraining user to enter string values wherever needed.
        print("\nThe Value should be String here")
        value = input(prompt)
    return value

#Creating function that will prompt user to enter Numeric Values & Specific Date Range only.
def restrictInput(prompt):
    value = input(prompt)
    while not value.isnumeric() or int(value)< 1915 or int(value)>2017:
        #Restraining user to enter numeric values wherever needed.
        print("\nThe Value should be numeric here or in the range of the avialable date range i.e 1916-2016")
        value = input(prompt)
    return int(value)

def restrictInput_dc(prompt):
    value = input(prompt)
    while not value.isnumeric() or int(value)<1 or int(value)>2399:
        #Restraining user to enter numeric values wherever needed.
        print("\nThe Value should be numeric here or valid range i.e between 1 to 2398 ")
        value = input(prompt)
    return int(value)

################################################################################################################################

#Main Menu Function Defn()

###############################################################################################################################

#Defining Main menu option which will be given to choose from 
def Defn():
    print("Please select one of the following options: \n1. Most successful Directors or Actors \n2. Film comparison \n3. Analyse the distribution of gross earnings \n4. Genre Analysis \n5.  Earnings and IMDB scores \n6. Exit")
    Action_to_perform = getInput("Enter Action required :")
    if Action_to_perform==1:
        Dir_Actor()
    elif Action_to_perform==2:
        Film_Compare()
    elif Action_to_perform==3:
        Analyse_gross()
    elif Action_to_perform==4:
        Genre_Analyse()
    elif Action_to_perform==5:
        Earnings_IMDB()
    elif Action_to_perform==6:
        print("exit")
    else:
        print("\n Please select from below options only: ")
        Defn()
        #Using Recursive function, calling main menu option whenever the user inputs options apart from the given options
        
###############################################################################################################################
        
#Q.1 Top Actors & Top Directors.

###############################################################################################################################

def Dir_Actor():
    Inner_Action = getInput("Please select one of the following options: \n1. Top Directors \n2. Top Actors \n3. Exit\n")
    if Inner_Action==1:
        Dir()
    elif Inner_Action==2:
        Act()
    elif Inner_Action==3:
        print("exit")
    else:
        print("\n Please select from below options only: ")
        Dir_Actor()

def Dir():
    Top1 = restrictInput_dc("Please Enter the list of Actors you wish to see :")
    out1=df_dict_act.groupby('director_name', as_index=False).agg({'gross':'sum'}).sort_values(by='gross',ascending=False)[:Top1]
    #out1 = df_dict_act[['director_name','gross']][:Top1]
    #filtering director_name & gross till the given input
    #print(out1).sort_values(by='gross',ascending=False)
    output1=out1.sort_values(by='gross',ascending=False)
    print(output1,"\n")
    fig, ax = plt.subplots()
    index = np.arange(len(out1['director_name']))
    #taking total number of directors in the fitered out1
    Directors = ax.barh(out1['director_name'], out1['gross'],label="Directors")
    #Using barh function to give horizontal bar as output
    plt.show()
    print("\n")
    Defn()
    
def Act():
    Top2 = restrictInput_dc("Please Enter the list of Actors you wish to see :")
    out2=df_dict_act.groupby('actor_1_name', as_index=False).agg({'gross':'sum'}).sort_values(by='gross',ascending=False)[:Top2]
    #out2 = df_dict_act[['actor_1_name','gross']][:Top2]
    #filtering director_name & gross till the given input
    output2=out2.sort_values(by='gross',ascending=False)
    print(output2,"\n")
    fig, ax = plt.subplots()
    index = np.arange(len(out2['actor_1_name']))
    #taking total number of actors in the fitered out2
    Actors = ax.barh(out2['actor_1_name'], out2['gross'],label="Actors")
    #Using barh function to give horizontal bar as output
    plt.show()
    print("\n")
    Defn()

###############################################################################################################################    

#Q.2 Film Comparison :

###############################################################################################################################

def Film_Compare():
    movie_names = []
    #array defintion
    while len(movie_names)<2:
        #the while loop goes on till the user inputs 2 movie names
        movie=str.lower(getstrInput("Please Enter the Movie Name :"))
        if movie == 'q':
            break
        if movie  not in df_new['movie_title'].values or movie in movie_names:
            #checking if movie name exists or not in both dataframe & created array.
            print("Not a valid name, Please enter again. press q to exit")
        else:
            movie_names.append(movie)
            #storing movie names
    try:
        first=movie_names[0]
        second=movie_names[1]
        print(first)
        print(second)
        print("\n")
        Inner_prompt(first,second)
    except Exception as e:
        print("\n")
        Defn()
        
    #Calling function & passing values of both movie names

#Inner Menu Option    
def Inner_prompt(first,second):
    check = getInput("Please select options from the list :\n1. IMDB Scores \n2. Gross Earning \n3. Movie Facebook Likes\n")
    if check==1:
        IMDB(first,second)
    elif check==2:
        Gross_earn(first,second)
    elif check==3:
        movie_fb(first,second)
    else:
        print("\n Please select from below options only: ")
        Inner_prompt(first,second)
        
def BackMenu(first,second):
    check=getInput("Please select the following option \n1. For Previous Menu\n2. For Main Menu\n3. For exit :\n")
    if check==1:
        Inner_prompt(first,second)
    elif check==2:
        Defn()
    elif check==3:
        print("exit")
#Back Menu provides inner back menu to check other options inside Film Campanion or Main menu        

#IMDB Score Comparison for 2 Films    
def IMDB(first,second):
    check1=df_new[df_new['movie_title']==first]
    #filtering data where movie matches the case
    print(first,":",check1['imdb_score'])
    check2=df_new[df_new['movie_title']==second]
    #filtering data where movie matches the case
    print(second,":",check2['imdb_score'])
    index = np.arange(1)
    bar_width = 0.35
    fig, ax = plt.subplots()
    Film1 = ax.bar(index, check1['imdb_score'], bar_width,label="Film1")
    #plotting bar chart for movie1
    Film2 = ax.bar(index+bar_width, check2['imdb_score'],bar_width, label="Film2")
    #plotting bar chart for movie2
    ax.legend()
    #providing legend to identify difference between both comparison
    plt.show()
    print("\n")
    
    check=getInput("Please select the following option \n1. For Previous Menu\n2. For Main Menu\n3. For exit :\n")
    if check==1:
        Inner_prompt(first,second)
    elif check==2:
        Defn()
    elif check==3:
        print("exit") 
    else:
        print("\n Please select from below options only: ")
        BackMenu(first,second)
        
#Gross Earning comparison for 2 films
def Gross_earn(first,second):
    check1=df_new[df_new['movie_title']==first]
    print(first,":",check1['gross'])
    check2=df_new[df_new['movie_title']==second]
    print(second,":",check2['gross'])
    index = np.arange(1)
    bar_width = 0.35
    fig, ax = plt.subplots()
    Film1 = ax.bar(index, check1['gross'], bar_width,label="Film1")
    #plotting bar chart for movie1
    Film2 = ax.bar(index+bar_width, check2['gross'],bar_width, label="Film2")
    #plotting bar chart for movie2
    ax.legend()
    #providing legend to identify difference between both comparison
    plt.show()
    print("\n")

    check=getInput("Please select the following option \n1. For Previous Menu\n2. For Main Menu\n3. For exit :\n")
    if check==1:
        Inner_prompt(first,second)
    elif check==2:
        Defn()
    elif check==3:
        print("exit") 
    else:
        print("\n Please select from below options only: ")
        BackMenu(first,second) 

#Movie Facebook Likes for 2 films
def movie_fb(first,second):
    check1=df_new[df_new['movie_title']==first]
    print(first,":",check1['movie_facebook_likes'])
    check2=df_new[df_new['movie_title']==second]
    print(second,":",check2['movie_facebook_likes'])
    index = np.arange(1)
    bar_width = 0.35
    fig, ax = plt.subplots()
    Film1 = ax.bar(index, check1['movie_facebook_likes'], bar_width,label="Film1")
    #plotting bar chart for movie1
    Film2 = ax.bar(index+bar_width, check2['movie_facebook_likes'],bar_width, label="Film2")
    #plotting bar chart for movie2
    ax.legend()
    #providing legend to identify difference between both comparison
    plt.show()
    print("\n")
    
    check=getInput("Please select the following option \n1. For Previous Menu\n2. For Main Menu\n3. For exit :\n")
    if check==1:
        Inner_prompt(first,second)
    elif check==2:
        Defn()
    elif check==3:
        print("exit") 
    else:
        print("\n Please select from below options only: ")
        BackMenu(first,second) 
        
################################################################################################################################ 

#Q.3 Analyse Gross Earnings based on start year & end year

###############################################################################################################################

def Analyse_gross():
    Start_year=restrictInput("Enter Start Year :")
    end_year=restrictInput("Enter End Year :")
    check=df_dict_act[df_dict_act['title_year'].between(Start_year, end_year, inclusive=False)]
    #filtering data lies in the range of start year & end year
    if check.empty:
        #to accept only valid inputs. 
        Analyse_gross()
    else:
        check.groupby(['title_year'])['gross'].min().plot(label="Minimum")
        check.groupby(['title_year'])['gross'].max().plot(label="Maximum")
        check.groupby(['title_year'])['gross'].mean().plot(label="Average")
        #group by on title year & min,max & mean of gross & plotting line graph
        plt.legend(loc = "best")
        #plotting the multiple line graphs
        plt.show() 
        print("\n")
        Defn()
        
#################################################################################################################################

#Q.4 Genre selection & respective Mean

###############################################################################################################################

def Genre_Analyse():
    test=df_new['genres'].str.split('|')
    #Replacing pipe seperated data to comma
    unique = set()
    for i in test:
        for j in i:
            unique.add(j)
            #Adding only unique column values that exists Genre column

    print("Choose from the following list of Genre",unique)
    Item=getstrInput("Enter Genre Name :")
    output=df_new[df_new['genres'].str.contains(Item.lower())]['imdb_score']
    #converting the input data genre from user to lower case
    output1=output.mean()
    #taking mean of the selected genre
    print("The mean for the selected Genre i.e",Item.lower(),"is as follows : ",output1,"\n")
    Defn()

################################################################################################################################

#Q.6 Predict IMDB score & model Accuracy

###############################################################################################################################

def Earnings_IMDB():
    check=df_new.dropna()
    score_model = ols("""imdb_score ~ num_critic_for_reviews + num_user_for_reviews + actor_1_facebook_likes + gross + duration + director_facebook_likes + actor_3_facebook_likes + num_voted_users + cast_total_facebook_likes + facenumber_in_poster + content_rating + budget + title_year + movie_facebook_likes + aspect_ratio""", data=check).fit()
    fig = plt.figure(figsize=(15,8))
    fig = sm.graphics.plot_regress_exog(score_model, "num_critic_for_reviews", fig=fig)
    score_model_summary = score_model.summary()
    plt.show()
    print(score_model_summary) 
    print("\n")
    Defn()
    
################################################################################################################################    

Defn()

