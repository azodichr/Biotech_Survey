"""
For all files in directory with _results.txt. Merge results into one master file.

Note - only works if all _results.txt files in that directory have the same classes

"""


import os, sys
import pandas as pd
import numpy as np
import re

classes = sys.argv[1].strip().split(',')
classes_col_names = []
for cl in classes:
	classes_col_names.append(cl + '_f1')
	classes_col_names.append(cl + '_f1_sd')

col_names = ['ID', 'Alg', 'NumFeat', 'MinSize', 'CVf', 'Params', 'Acc', 'Acc_sd', 'F1m', 'F1m_sd'] + classes_col_names
results = pd.DataFrame(columns = col_names)

files = [f for f in os.listdir('.') if os.path.isfile(f)]
for f in files:
	if f.startswith("."):
		pass
	elif '_results.txt' in f:
		with open(f) as file_:
			tmp_f1cl_dict = {}
			tmp_f1clsd_dict = {}
			for l in file_:
				if l.startswith('ID'):
					tmp_id = l.strip().split(': ')[1]
				elif l.startswith('Alg'):
					tmp_alg = l.strip().split(': ')[1]
				elif l.startswith('Number of features'):
					tmp_NumFeat = l.strip().split(': ')[1]
				elif l.startswith('Min class size'):
					tmp_BalSize = l.strip().split(': ')[1]
				elif l.startswith('CV folds'):
					tmp_CVf = l.strip().split(': ')[1]
				elif l.startswith('Parameters'):
					tmp_params = l.strip().split(':')[1]
				elif l.startswith('Accuracy'):
					tmp_acc, tmp_accsd = l.strip().split('\t')[1:]
				elif l.startswith('F1_macro'):
					tmp_f1, tmp_f1sd = l.strip().split('\t')[1:]
				elif '_F1' in l:
					tmp_cl, tmp_f1cl, tmp_f1sdcl = l.strip().split('\t')
					tmp_cl = tmp_cl.replace('_F1', '')
					tmp_f1cl_dict[tmp_cl] = tmp_f1cl
					tmp_f1clsd_dict[tmp_cl] = tmp_f1sdcl

			tmp_list = [tmp_id, tmp_alg, tmp_NumFeat, tmp_BalSize, tmp_CVf, tmp_params, tmp_acc, tmp_accsd, tmp_f1, tmp_f1sd]
			tmp_list2 = []
			for i in classes_col_names:
				i_cl = i.strip().split('_')[0]
				if '_sd' in i:
					tmp_list2.append(tmp_f1clsd_dict[i_cl])
				else:
					tmp_list2.append(tmp_f1cl_dict[i_cl])

			tmp_df = pd.DataFrame([tmp_list + tmp_list2], columns = col_names)
			results = results.append(tmp_df)


print(results.head())

results.to_csv('RESULTS_multiclass.csv', sep=',')

