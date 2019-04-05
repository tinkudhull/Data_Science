# -*- coding: utf-8 -*-
"""
Created on Fri Jan 11 10:26:46 2019

@author: TI20018372
"""

import re
import glob
import pandas as pd

entities=['Premises Condition','Comment On Mobile Equipment Used',
          'Insured Sells Liquor','Lighting','Undesirable Condition',
          'Exterior Work Performed Above Two Stories In',
          'Equipment Loaned/Rented/Leased To',
          'Are Cranes / Helicopters Utilized','Any Operations/Service Provided Off',
          "COl's From All Suppliers/Subs",'ANSI Z 535 Used For Warnings',
          'Products Sold Under Others Labels','Customer Complaints and Incidents',
          'Any Prior Losses or Claims in Past 3 Years']

all_f = glob.glob("D:\DAAI\data_08_01\onedrive\outputs\*\INSP_radio.txt")
no_of_folders = len(all_f)
#output = pd.DataFrame(columns=entities, index = range(0,1))
output = pd.DataFrame()

for i in range(0,no_of_folders):
    file_name=all_f[i]
    
    try:
        with open(file_name,'r',encoding='utf8') as f:
            content = f.readlines()
    except IOError:
        print("could not read file:", file_name)
    no_of_lines=len(content)
def extract_entity(entity):
    entity = entity.lower()
    index = [x for x in range(no_of_lines) if entity in content[x].lower()]
    if (entity == 'comment on mobile equipment used'):
        indd = [x for x in range(no_of_lines) if content[x].lower().startswith('comment on mobile equipment used')]
        if indd:
            indd2 = [x for x in range(indd[0],no_of_lines) if content[x].lower().startswith('comments')]
            comment_flag="No"
            for q in range(indd[0],indd2[0]):
                if ("cranes" in content[q].lower().split()):
                    comment_flag="Yes"
                    break
            print(' ',comment_flag,'\n')
        else:
            print(" Entity not found \n")
    else:
        if index:
            if (entity == 'lighting'):
                index = [x for x in range(no_of_lines) if content[x].lower().startswith(entity)]
            i=1
            line = content[index[0]]
            while True:
                if ":" in line:
                    ans=line.split(":")
                    res=ans[-1]
                    # res=''.join(e for e in ans[-1] if e.isalnum())
                    res=re.sub(r"e\)|s\)|a\)|\)|\\|&\)|[0-9]|b\)","",str(res))
                    #res=re.sub(r"[^a-zA-Z]"," ",str(res))
                    print(res)
                    return res.strip('\n')
                else:
                    pass
                i=i+1
                line=content[index[0]+i]
        else:
            print(" Entity not found")
        
for i in range(0,no_of_folders):
    dict1 = {}
    for e in entities:
        #print(e, end='')
        dict1[e]=extract_entity(e)
    out = pd.DataFrame.from_dict(dict1, orient = 'index')
    ttt = out.T
    output = output.append(ttt)

print(output)
#output.to_csv("D:\DAAI\my outputs\fo.csv")
