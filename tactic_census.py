import os
import json
from collections import Counter

"""
This code counts the total number of invocations of each tactic in `Mathlib` by
parsing the data generated from the tactic extraction script.
This code was written with help from ChatGPT.
"""

folder_path = 'TacticExtractionData'  # Specify the folder path where your .json files are located
output_file = 'tactic_census.json'  # Specify the output file name

# Initialize a counter to keep track of the occurrences of each value
value_counter = Counter()

# Iterate over each .json file in the folder
for filename in os.listdir(folder_path):
    if filename.endswith('.json'):
        file_path = os.path.join(folder_path, filename)
        with open(file_path) as file:
            json_data = json.load(file)
            # Iterate over each object in the JSON array
            for obj in json_data:
                tactic_name = obj.get('mainTactic')  # Get the value of 'abc' field
                if tactic_name not in [None, "none"]:
                    value_counter[tactic_name] += 1

# Prepare the output data as a list of dictionaries
output_data = [{'tactic': value, 'count': count} for value, count in value_counter.items()]
output_data.sort(key=lambda x: x['count'], reverse=True)

# Write the output data to a JSON file
with open(output_file, 'w') as file:
    json.dump(output_data, file, indent=2, ensure_ascii=False)

print(f'Successfully written output to {output_file}.')
