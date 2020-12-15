import os
import pathlib


def speak_nums(nums, target):
    memo = {}
    i = 0
    while i < len(nums)-1:
        memo[nums[i]] = i
        i += 1
    curr = nums[i]
    while i < target-1:
        if curr not in memo:
            next_num = 0
        else:
            next_num = i-memo[curr]
        memo[curr] = i
        curr = next_num
        i += 1
    return curr



if __name__ == "__main__":
    print(speak_nums([20,9,11,0,1,2], 2020))
    print(speak_nums([20,9,11,0,1,2], 30000000))
