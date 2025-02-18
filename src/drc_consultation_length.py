# Analysis of the consultation length of antenatal appointments in DRC
# This is Question 2.20a in the f3_do_anc.xlsx
import pandas as pd

infile = "../resources/drc/follow-up-survey/" \
    "COD_2021-2022_HRBFIE-HFFU_v01_M_CSV/f3_f5_anc_observation_exit_anon.csv"

# Encoding detected using chardet
drc_followup = pd.read_csv(infile, encoding="cp1252", encoding_errors='ignore')

# Sanity checks; 3 values below 0. I think we can assume these were meant to be
# positive
check = drc_followup['f3_220a'] < 0
print(drc_followup.loc[check, 'f3_220a'])

drc_followup.loc[check, 'f3_220a'] = -1 * drc_followup.loc[check, 'f3_220a']
