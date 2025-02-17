import pandas as pd
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


infile = "resources/drc/f3_do_anc_tmp.xlsx"
outfile = "resources/drc/do_anc.csv"
xls = pd.ExcelFile(infile)
sheets = xls.sheet_names
# First sheet is cover page, we don't need to raed it.
# Second sheet records information about the surveyor
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
en_df.to_csv(outfile, index=False, encoding="utf-8")
