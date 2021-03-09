
# fantastic python package!
# https://github.com/Knio/pynmea2
import pynmea2
import json

# there must be a cleaner way to do this kind of thing...
all_names = dir(pynmea2)
sentence_classes = [getattr(pynmea2, x) for x in all_names if not x.startswith("_") and  type(getattr(pynmea2, x)) is pynmea2.nmea.NMEASentenceType]


# assemble all the fields, transforming functions to their name
field_dict = {}
for sentence_class in sentence_classes:
    fields = list([list(x) for x in sentence_class.fields])
    for i, field in enumerate(sentence_class.fields):
        for j, value in enumerate(sentence_class.fields[i]):
            if callable(value):
                fields[i][j] = value.__name__
        
    field_dict[sentence_class.__name__] = fields

# dump as JSON
with open("nmea-fields.json", "w") as f:
    json.dump(field_dict, f)
