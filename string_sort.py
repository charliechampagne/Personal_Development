def solution(s):
    n = len(s)
    s_len = n
    lst = []
    new_list = []
    if n % 2 == 0:
        for i in range(n):
            lst.append(s[i])
        times = n // 2
        for i in range(times):
            temp1 = []
            temp1.append(lst[1])
            temp1.append(lst[0])
            for j in range(2):
                lst.pop(0)
                new_list.append(temp1[j])
    else:
        n -= 1
        times = n // 2
        for i in range(n):
            lst.append(s[i])
        for i in range(times):
            temp1 = []
            temp1.append(lst[1])
            temp1.append(lst[0])
            for j in range(2):
                lst.pop(0)
                new_list.append(temp1[j])
        new_list.append(s[-1])

    output_str = "".join(new_list)

    return output_str

s = 'abcdefgh'
print(solution(s))

