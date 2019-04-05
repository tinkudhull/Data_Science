# -*- coding: utf-8 -*-
"""
Created on Wed Mar  6 12:09:03 2019

@author: TI20018372
"""
import glob
import os
import re
import pandas as pd
import babel.numbers

def main():
    input_path = "D:\\DAAI\\Audit entities\\output_12272018_output\\output_101_exl_modified\\103 GL 0016957-00"
    output = pd.DataFrame(columns = ['Entity', 'Premium Audit Report', 'Risk Evaluation Report', 'Binder', 'Endorsement', 'Cancellation', 'ACORD', 'Flag'])
    os.chdir(input_path)
    file_list = os.listdir()
    dictionary = audit_ext(file_list)
    j=0
    binder_df = pd.DataFrame(columns = ['cc','exposure', 'rb', 'rate', 'premium'])
    endorse_df = pd.DataFrame(columns = ['cc','exposure', 'rb', 'rate', 'premium'])
    for key,value in dictionary.items():
        if "I" in key:
            output.loc[j,"Entity"] = "Class Code Insured: Exposure Amount"
        elif "U" in key:
            output.loc[j,"Entity"] = "Class Code Uninsured: Exposure Amount"
        else:
            output.loc[j,"Entity"] = "Class Code: Exposure Amount"
        key_a = babel.numbers.format_currency( float(value), "USD",locale='en_US' ) 
        output.loc[j,"Premium Audit Report"] = '[class code] : '.join([key, key_a])
        a,b,c,d = binder_ext(file_list, key)
        binder_df = binder_df.append({'cc':key,'exposure':a, 'rb':b, 'rate':c, 'premium':d}, ignore_index = True)
        temp = key + ': ' + a + ' [' + b + '] ' + c + ' [Rate] ' + d + ' [Premium]'
        temp = temp.replace('$0IA', 'If Any')
        temp = temp.replace('$0I', 'Included')
        output.loc[j,"Binder"] = temp
        aa,bb,cc,dd = endorse_ext(key)
        tempp = key + ': ' + aa + ' [' + bb + '] ' + cc + ' [Rate] ' + dd + ' [Premium]'
        if aa==bb==cc==dd=='na':
            output.loc[j,"Endorsement"] = "na"
        else:
            output.loc[j,"Endorsement"] = tempp
            endorse_df = endorse_df.append({'cc':key,'exposure':aa, 'rb':bb, 'rate':cc, 'premium':dd}, ignore_index = True)
        if a==b==c==d==aa==bb==cc=="na":
            output.loc[j,"Flag"] = 1
        else:
            output.loc[j,"Flag"] = 0
        j = j+1
        
    
    yz = binder_ext_2(file_list)
    zz = endorse_ext_2()
    abc = calculation(yz,zz)
    zz = zz.replace({'\$|,': ''}, regex=True)
    abcd = pd.concat([abc,zz], axis=0)
    abcd.drop_duplicates(inplace = True)
    abcd['premium'] = abcd['premium'].astype('float64') 
    total_bound_premium = float(abcd['premium'].sum())
    final_df = calculation(binder_df, endorse_df)
    audit_sum = sum(dictionary.values())
    binder_sum = final_df['exposure'].sum()
    if audit_sum == binder_sum:
        audit_waiver = 1
    else:
        audit_waiver = 0
    if audit_waiver == 0:
        total_audited_premium = 0
        for key,value in dictionary.items():
            audited_premium = complex_c(key,value,final_df)
            total_audited_premium = total_audited_premium + audited_premium
    if total_audited_premium < total_bound_premium :
        output.loc[-1,"Premium Audit Report"] = "Issue Audit Waiver - $0.00"
    else:
        add_premium = total_audited_premium - total_bound_premium
        if add_premium > 100:
            add_premium =  babel.numbers.format_currency( add_premium, "USD",locale='en_US')
            out_str = "Issue Audit Premium of " + add_premium
            output.loc[-1,"Premium Audit Report"] = out_str
        else:
            output.loc[-1,"Premium Audit Report"] = "Issue Audit Waiver - $0.00"
    output.loc[-1,"Entity"] = "Audit Premium Decision"
    exclusions = audit_ext_exclusion(file_list)
    for key,value in exclusions.items():
        output.loc[j,"Entity"] = "Exclusions"
        output.loc[j,"Premium Audit Report"] = key + " : " + value
        output.loc[j,"Flag"] = exclusion_flag(key)
        j = j+1
    output.loc[j,"Entity"] = "Contractors: OSHA Checked"
    output.loc[j,"Flag"] = osha_check(file_list)
    output.sort_index(inplace = True)
    output.fillna(value = "na", inplace = True)
    writer = pd.ExcelWriter('D:\\DAAI\\my outputs\\audits\\New folder\\103 GL 0016957-00.xlsx', engine='xlsxwriter')
    output.to_excel(writer, sheet_name = 'Sheet1')
    workbook = writer.book
    worksheet = writer.sheets['Sheet1']
    format1 = workbook.add_format({'bg_color':   'green'})
    worksheet.conditional_format('B2:I2', {'type': 'cell','criteria':'equal to','value':'B2','format':format1})
    writer.save()

def audit_ext(file_list):
    audit_flag = 1
    if ('AUDF.txt' in file_list):
        with open("AUDF.txt",'r',encoding='utf8') as a:
            audit_content = a.readlines()
    else:
        audit_flag = 0
    dictionary = dict()
    if audit_flag == 1:
        no_of_lines = len(audit_content)
        index = [x for x in range(no_of_lines) if audit_content[x].lower().startswith("recap summary")]
        if index:
            ss = audit_content[index[0]+1]
            audit_class_code = re.findall(r'(?:[0-9]{5}).*$', ss)
            audit_cc = []
            for x in audit_class_code:
                audit_cc.extend(x.split())
            audit_cc = audit_cc[:-1]
            for n,elements in enumerate(audit_cc):
                if len(elements) == 6:
                    if "U" not in elements:
                        if "I" not in elements:
                            n_e = elements[:-1]
                            n_e = n_e + "I"
                            audit_cc[n] = n_e
            index_2 = [x for x in range(index[0], index[0]+10) if audit_content[x].lower().startswith("total")]
            def total_ext(index_2):
                y = audit_content[index_2].split()[1:-1]
                yy = [yy.replace(',','') for yy in y]
                yy = [float(i) for i in yy]
                return yy
            if (len(index_2) == 1):
                total_value = total_ext(index_2[0])
            elif(len(index_2) == 2):
                total_value = [x + y for x, y in zip(total_ext(index_2[0]), total_ext(index_2[1]))]
            else:
                total_value = [x + y + z for x, y, z in zip(total_ext(index_2[0]), total_ext(index_2[1]), total_ext(index_2[2]))]
            dictionary = dict(zip(audit_cc, total_value))
            if 'EXCL' in dictionary:
                del dictionary['EXCL']
            return dictionary
        else:
            return dictionary
    else:
        return dictionary


def audit_ext_exclusion(file_list):
    audit_flag = 1
    if ('AUDF.txt' in file_list):
        with open("AUDF.txt",'r',encoding='utf8') as a:
            audit_content = a.readlines()
    else:
        audit_flag = 0
    word_dict = {}
    if audit_flag == 1:
        no_of_lines = len(audit_content)
        index = [x for x in range(no_of_lines) if audit_content[x].lower().startswith("recap summary")]
        if index:
            index2 = [x for x in range(index[0],(index[0]+15)) if audit_content[x].startswith("AUDIT")]
            if index2:
                for jj in range(index[0]+1, index2[0]):
                    ss = audit_content[jj]
                    audit_class_code_v = re.findall(r'(?:-[0-9]{1,})', ss)
                    if len(audit_class_code_v) > 0:
                        word = re.findall(r"^(.+?)\s-", ss)[0]
                        val = babel.numbers.format_currency( float(audit_class_code_v[0]), "USD",locale='en_US' )
                        word_dict[word] = val
    return word_dict

def osha_check(file_list):
    audit_flag = 1
    if ('AUDF.txt' in file_list):
        with open("AUDF.txt",'r',encoding='utf8') as a:
            audit_content = a.readlines()
    else:
        audit_flag = 0
    osha_flag = 0
    if audit_flag == 1:
        no_of_lines = len(audit_content)
        index = [x for x in range(no_of_lines) if audit_content[x].lower().startswith("business overview")]
        if index:
            for i in range(index[0], no_of_lines):
                if "OSHA violation" in audit_content[i]:
                    osha_flag = 1
                    return osha_flag
    return osha_flag


def exclusion_flag(name):
    if "Intercompany Sales" in name:
        flag = 0
    elif "OCIP" in name:
        flag = 0
    elif "Tax" in name:
        flag = 0
    elif "Freight" in name:
        flag = 1
    else:
        flag = 1
        with open("AUDF.txt",'r',encoding='utf8') as a:
            audit_content = a.readlines()
        no_of_lines = len(audit_content)
        index = [x for x in range(no_of_lines) if audit_content[x].lower().startswith("business overview")]
        if index:
            for k in range(index[0], no_of_lines):
                if name.lower() in audit_content[k].lower():
                    flag = 0
                    break
    return flag


def binder_ext(file_list,acc):
    binder_flag = 1
    if ('BNDC.txt' in file_list):
        with open("BNDC.txt",'r',encoding='utf8') as b:
            binder_content = b.readlines()
    else:
        binder_flag = 0
    if binder_flag == 1:
        no_of_lines = len(binder_content)
        indexx = [x for x in range(no_of_lines) if binder_content[x].lower().startswith("premium basis")]
        if indexx:
            indexx2 = [x for x in range(indexx[0],no_of_lines) if binder_content[x].lower().startswith("page")]
            check_for_u = 0
            if "I" in acc:
                y = acc[:-1]
            elif "U" in acc:
                check_for_u = 1
                y = acc[:-1]
            else:
                y = acc
            indexx3 = [x for x in range(indexx[0],indexx2[0]) if binder_content[x].lower().startswith(y)]
            if indexx3:
                for ii in range(0,len(indexx3)):
                    s = binder_content[indexx3[ii]]
                    if len(indexx3) > 1:
                        if y==acc:
                            if "Sub Cost" in s:
                                continue
                    if check_for_u == 1:
                        ss = binder_content[indexx3[0]+1]
                        if "U252" not in ss:
                            return ["na","na","na","na"]
                    s = s.replace('If Any', '$0IA')
                    s = s.replace('Included', '$0I')
                    all_in = re.findall(r'(\$.*?)(?:\s|$|\-)', s)
                    if (len(all_in) == 3):
                        binder_exposure = all_in[0]
                        binder_rate = all_in[1]
                        binder_premium = all_in[2]
                    else:
                        binder_exposure = ""
                        binder_rate = ""
                        binder_premium = ""
                    try:
                        binder_rating_basis = rb_ext(s)
                    except:
                        binder_rating_basis = ""
                    return[binder_exposure,binder_rating_basis, binder_rate, binder_premium]
            else:
                return ["na","na","na","na"]
        else:
            return ["na","na","na","na"]
    else:
        return ["na","na","na","na"]


def rb_ext(s):
    if "Gross Sales" in s:
        rb = "Gross Sales"
    elif "Sub Cost" in s:
        rb = "Sub Cost"
    else:
        rb = "Payroll"
    return rb
    

def binder_ext_2(file_list):
    binder_flag = 1
    if ('BNDC.txt' in file_list):
        with open("BNDC.txt",'r',encoding='utf8') as b:
            binder_content = b.readlines()
    else:
        binder_flag = 0
    binder_dff = pd.DataFrame(columns = ['cc','exposure', 'rb','rate', 'premium'])
    if binder_flag == 1:
        no_of_lines = len(binder_content)
        indexx = [x for x in range(no_of_lines) if binder_content[x].lower().startswith("premium basis")]
        if indexx:
            indexx2 = [x for x in range(indexx[0],no_of_lines) if binder_content[x].lower().startswith("page")]
            for i in range(indexx[0]+1, indexx2[0]):
                ab = re.findall(r'(^[0-9]{5})\s',binder_content[i])
                if len(ab) > 0:
                    s = binder_content[i]
                    s = s.replace('If Any', '$0')
                    s = s.replace('Included', '$0')
                    all_in = re.findall(r'(\$.*?)(?:\s|$|\-)', s)
                    try:
                        a1 = all_in[0]
                        a2 = all_in[1]
                        a3 = all_in[2]
                    except:
                        a1 = ""
                        a2 = ""
                        a3 = ""
                    rb = rb_ext(s)
                    binder_dff = binder_dff.append({'cc':ab[0], 'exposure':a1,'rb':rb, 'rate':a2,'premium':a3}, ignore_index=True)
                    
    return binder_dff
    
def endorse_ext_2():
    all_endf = glob.glob("ENDF*.txt")
    endorse_dff = pd.DataFrame(columns = ['cc','exposure', 'rb','rate', 'premium'])
    for t in range(0,len(all_endf)):
        with open(all_endf[t],'r',encoding='utf8') as e:
            endorse2_content = e.readlines()
            no_of_lines = len(endorse2_content)
            index = [x for x in range(no_of_lines) if "code no" in endorse2_content[x].lower()]
            if index:
                for j in range(0,len(index)):
                    try:
                        s = endorse2_content[index[j]]
                        endorse2_cc = re.findall(r'([0-9]{5})',s)[0]
                        endorse2_exposure = re.findall(r'\s(\$.*?)(?:\s|$)', endorse2_content[index[j]+1])[0]
                        endorse2_rate = re.findall(r'\s(\$.*?)(?:\s|$)', endorse2_content[index[j]+2])[0]
                        endorse2_premium = re.findall(r'\s(\$.*?)(?:\s|$)', endorse2_content[index[j]+3])[0]
                        ss = endorse2_content[index[j]+1]
                        endorse2_rating_basis = rb_ext(ss)
                        endorse_dff = endorse_dff.append({'cc':endorse2_cc, 'exposure':endorse2_exposure,'rb':endorse2_rating_basis, 'rate':endorse2_rate,'premium':endorse2_premium}, ignore_index=True)
                    except:
                        pass
    return endorse_dff

def endorse_ext(acc):
    all_endf = glob.glob("ENDF*.txt")
    for t in range(0,len(all_endf)):
        with open(all_endf[t],'r',encoding='utf8') as e:
            endorse2_content = e.readlines()
            no_of_lines = len(endorse2_content)
            index = [x for x in range(no_of_lines) if "code no" in endorse2_content[x].lower()]
            if index:
                s = endorse2_content[index[0]]
                if acc in s:
                    try:
                        endorse2_exposure = re.findall(r'\s(\$.*?)(?:\s|$)', endorse2_content[index[0]+1])[0]
                        endorse2_rate = re.findall(r'\s(\$.*?)(?:\s|$)', endorse2_content[index[0]+2])[0]
                        endorse2_premium = re.findall(r'\s(\$.*?)(?:\s|$)', endorse2_content[index[0]+3])[0]
                        ss = endorse2_content[index[0]+1]
                        endorse2_rating_basis = rb_ext(ss)
                    except:
                        pass
            else:
                endorse2_exposure = "na"
                endorse2_rate = "na"
                endorse2_premium = "na"
                endorse2_rating_basis = "na"
 
    return[endorse2_exposure,endorse2_rating_basis, endorse2_rate, endorse2_premium]

def calculation(binder_df, endorse_df):
    final_df = binder_df.copy()
    for i in range(0,binder_df.shape[0]):
        for j in range(0,endorse_df.shape[0]):
            if binder_df.loc[i,'cc'] == endorse_df.loc[j,'cc']:
                final_df.loc[i,:] = endorse_df.loc[j,:].values
                
    final_df = final_df.replace({'\$0IA|\$0I': '0'}, regex=True)
    final_df = final_df.replace({'\$|,': ''}, regex=True)
    return final_df

def complex_c(key,value,final_df):
    bin_end_pre = 0
    temp = value
    temp2 = final_df.loc[final_df['cc'] == key, 'rate'].item()
    final_df2 = final_df.fillna(0)
    if "I" in key:
        tt = final_df2[['rb']][final_df2.premium == final_df2.premium.max()].values
        if "Gross Sales" in tt:
            return (0)
    temp3 = 0
    if temp2 == 'na':
        bin_end_pre += 0
    else :
        temp3 = temp*float(temp2)/1000
        bin_end_pre += 1
    audited_premium = temp3
    if bin_end_pre == 0:
        return(0)
    else:
        return(audited_premium)

if __name__ == "__main__":
    main()
