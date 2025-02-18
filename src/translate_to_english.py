import pandas as pd
import re
from transformers import MarianMTModel, MarianTokenizer

# Load the MarianMT model for French to English translation
model_name = "Helsinki-NLP/opus-mt-fr-en"
tokenizer = MarianTokenizer.from_pretrained(model_name)
model = MarianMTModel.from_pretrained(model_name)


def translate_text(text):
    """Translate French text to English using MarianMT."""
    if pd.isna(text):  # If the cell is empty, return as is
        return text
    batch = tokenizer([str(text)], return_tensors="pt", truncation=True)
    translated = model.generate(**batch)
    translated_text = tokenizer.decode(translated[0], skip_special_tokens=True)
    return translated_text


infile = "../resources/drc/f3_do_anc_tmp.xlsx"
outfile = "../resources/drc/do_anc.csv"
xls = pd.ExcelFile(infile)
sheets = xls.sheet_names

# First sheet is cover page, we don't need to read it.
# Second sheet records information about the surveyor and the patient
df = pd.read_excel(xls, sheet_name="1 Identification")
# Read in and translate questions one-by-one
# Note: lots of these questions have the abbrevation CPN
# (here translated to NPC)
# which means Normal Prenatal Consultations in English.
mask = df[
    "Section 1 : Identification du prestataire de santé et de la gestante"
].notna()
questions = df.loc[
    mask, "Section 1 : Identification du prestataire de santé et de la gestante"
]
questions = questions.astype(str).str.replace(r"[()\-]", "", regex=True)
en_answers = df.loc[mask, "Unnamed: 1"].apply(translate_text)
en_df = pd.concat([questions, en_answers], axis=1)
en_df = en_df.rename(
    columns={
        "Section 1 : Identification du prestataire de santé et de la gestante": "question_id",
        "Unnamed: 1": "question_text",
    }
)

########################################################
### Third sheet: this the set of questions used for DCO
def format_if_number(x):
    formatted = x
    if isinstance(x, float):
        formatted = "{:.2f}".format(x)
    return formatted


df = pd.read_excel(xls, sheet_name='2 Observation')
questions = df["Section 2 : Observation de la consultation prénatale"]

mask = questions.notna()
# Also remove Row 1 i.e., second row as it is an instruction
mask[1] = False

questions = questions[mask]
questions = questions.apply(format_if_number)

# Remove parens and negative sign
questions = questions.str.replace(r"[()\-]", "", regex=True)


current_number = None
result = []
number_pattern = re.compile(r"^-?\d+(\.\d+)?$")

# Iterate through each element in the Series
for item in questions[0:132]:
    # Check if the item is a number (including negatives) using the regex pattern
    if number_pattern.match(item):
        current_number = item
        result.append(item)
    else:
        # If it's not a number, treat it as a letter
        result.append(f"{current_number}{item.lower()}")

# Convert the result back to a Pandas Series
converted_series = pd.Series(result)

questions = pd.concat([converted_series, questions[132:]])

en_answers = df.loc[mask, "Unnamed: 1"].apply(translate_text)
en_df = pd.concat([questions, en_answers], axis=1)
en_df = en_df.rename(
    columns={
        "Section 2 : Observation de la consultation prénatale": "question_id",
        "Unnamed: 1": "question_text"
    }
)



### Finally write
en_df.to_csv(outfile, index=False, encoding="utf-8", mode='a', header=False)
