#--- Permutation Checker ---#
def rotate_right(permutation, n):
    n = n & 31
    return permutation[n:] + permutation[:n]

def rotate_left(permutation, n):
    return rotate_right(permutation, 32 - n)

def grevi(permutation, imm):
    new_perm = list(permutation)
    for i in range(32):
        old_index = i
        if (imm & (1 << 4)) != 0:
            old_index = (old_index - 16) if (old_index >= 16) else (old_index + 16)
        if (imm & (1 << 3)) != 0:
            old_index = (old_index - 8) if (old_index % 16 >= 8) else (old_index + 8)
        if (imm & (1 << 2)) != 0:
            old_index = (old_index - 4) if (old_index % 8 >= 4) else (old_index + 4)
        if (imm & (1 << 1)) != 0:
            old_index = (old_index - 2) if (old_index % 4 >= 2) else (old_index + 2)
        if (imm & (1 << 0)) != 0:
            old_index = (old_index - 1) if (old_index % 2 == 1) else (old_index + 1)
        
        new_perm[i] = permutation[old_index]
    return new_perm

def shfli(permutation, imm):
    new_perm = list(permutation)
    for i in range(32):
        old_index = i
        if (imm & (1 << 0)) != 0:
            group = old_index % 4
            if group == 0:
                old_index = old_index
            elif group == 1:
                old_index = old_index + 1
            elif group == 2:
                old_index = old_index - 1
            elif group == 3:
                old_index = old_index 
        if (imm & (1 << 1)) != 0:
            group = (old_index % 8) // 2
            if group == 0:
                old_index = old_index
            elif group == 1:
                old_index = old_index + 2
            elif group == 2:
                old_index = old_index - 2
            elif group == 3:
                old_index = old_index 
        if (imm & (1 << 2)) != 0:
            group = (old_index % 16) // 4
            if group == 0:
                old_index = old_index
            elif group == 1:
                old_index = old_index + 4
            elif group == 2:
                old_index = old_index - 4
            elif group == 3:
                old_index = old_index 
        if (imm & (1 << 3)) != 0:
            group = (old_index % 32) // 8
            if group == 0:
                old_index = old_index
            elif group == 1:
                old_index = old_index + 8
            elif group == 2:
                old_index = old_index - 8
            elif group == 3:
                old_index = old_index 
        new_perm[i] = permutation[old_index]
    return new_perm

def unshfli(permutation, imm):
    new_perm = list(permutation)
    for i in range(32):
        old_index = i
        if (imm & (1 << 3)) != 0:
            group = (old_index % 32) // 8
            if group == 0:
                old_index = old_index
            elif group == 1:
                old_index = old_index + 8
            elif group == 2:
                old_index = old_index - 8
            elif group == 3:
                old_index = old_index 
        if (imm & (1 << 2)) != 0:
            group = (old_index % 16) // 4
            if group == 0:
                old_index = old_index
            elif group == 1:
                old_index = old_index + 4
            elif group == 2:
                old_index = old_index - 4
            elif group == 3:
                old_index = old_index 
        if (imm & (1 << 1)) != 0:
            group = (old_index % 8) // 2
            if group == 0:
                old_index = old_index
            elif group == 1:
                old_index = old_index + 2
            elif group == 2:
                old_index = old_index - 2
            elif group == 3:
                old_index = old_index 
        if (imm & (1 << 0)) != 0:
            group = old_index % 4
            if group == 0:
                old_index = old_index
            elif group == 1:
                old_index = old_index + 1
            elif group == 2:
                old_index = old_index - 1
            elif group == 3:
                old_index = old_index 
        new_perm[i] = permutation[old_index]
    return new_perm

def parse_instruction(instr, perm):
    parts = instr.split()
    opcode = parts[0]
    imm_str = parts[3]
    imm = int(imm_str, 0)
    
    if opcode == "rori":
        return rotate_right(perm, imm)
    elif opcode == "roli":
        return rotate_left(perm, imm)
    elif opcode == "grevi":
        return grevi(perm, imm)
    elif opcode == "shfli":
        return shfli(perm, imm)
    elif opcode == "unshfli":
        return unshfli(perm, imm)
    else:
        raise ValueError(f"Unknown opcode: {opcode}")

def emulate(instructions, initial_perm=None):
    if initial_perm is None:
        perm = list(range(32))
    else:
        perm = list(initial_perm)

    for instr in instructions:
        perm = parse_instruction(instr, perm)
    return perm

def get_mirr_permutation(perm: list[int]) -> list[int]:
    n = len(perm)
    mirr_perm = [0] * n

    for i in range(n):
        orig_ith = perm[i]
        mirr_idx = n - 1 - i
        mirr_val = n - 1 - orig_ith
        mirr_perm[mirr_idx] = mirr_val
    return mirr_perm


def main():
    target_perms = list(range(32))
    print("Original:   ", target_perms)
    target_perms[1] = 4
    target_perms[4] = 1

    print("Unmirrored: ", target_perms)
    # target_perms = get_mirr_permutation(target_perms)
    # print("Mirrored:   ", target_perms)
    print("Searching for instructions to swap selected bits...")

    found_instructions = [
        "shfli t0, t0, 0x2",
        "rori t0, t0, 1",
        "unshfli t0, t0, 0xf",
        "rori t0, t0, 1",
        "shfli t0, t0, 0xf",
        "rori t0, t0, 29",
        "shfli t0, t0, 0x2",
        ]
    if found_instructions:
        print("\nFound instruction sequence:")
        for instr in found_instructions:
            print(instr)
        
        result_perm = emulate(found_instructions)
        if result_perm == target_perms:
            print("\nVerification: Sequence successfully swaps bits.")
            print("Expected:", target_perms)
            print("Result:  ", result_perm)
        else:
            print("\nVerification: Sequence found, but does not match target.")
            print("Expected:", target_perms)
            print("Result:  ", result_perm)
    else:
        print("\nNo sequence found within the specified depth.")

if __name__ == "__main__":
    main()