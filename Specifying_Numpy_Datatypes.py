import numpy as np
studType = np.dtype([('name', str), ('age', int), ('height', float)])
students = [('James', 5, 48.5), ('Neil', 6, 52.5), ('Pauline', 0, 42.10)]
arr = np.array(students, dtype = studType)
newarr = np.sort(arr, order = 'height')
print(newarr)

