#include <iostream>
#include <vector>
#include <string>
#include <deque>
#include <sstream>
#include <algorithm>
#include <map>
#include <unordered_set>
#include <unordered_map>
#include <tuple>
#include <chrono>
#include <array>
#include <functional>

using Permutation = std::array<int, 32>;

struct PermutationHasher {
    size_t operator()(const Permutation& p) const {
        size_t hash = 0;
        for (int x : p) {
            hash ^= std::hash<int>()(x);
            hash *= 0x100000001b3;
        }
        return hash;
    }
};

struct OperationInfo {
    std::string name;
    std::string type;
    int imm;
    std::function<Permutation(const Permutation&)> func;
    std::function<Permutation(const Permutation&)> reverse_func;

    OperationInfo() : name(""), type(""), imm(0), func([](const Permutation& p){return p;}), reverse_func([](const Permutation& p){return p;}) {}
    OperationInfo(const std::string& n, const std::string& t, int i,
                  std::function<Permutation(const Permutation&)> f,
                  std::function<Permutation(const Permutation&)> rf)
        : name(n), type(t), imm(i), func(f), reverse_func(rf) {}
};

struct State {
    Permutation perm;
    std::vector<std::string> path;
    std::string last_op_type;
    int last_op_imm;

    State(const Permutation& p, const std::vector<std::string>& pa)
        : perm(p), path(pa), last_op_type(""), last_op_imm(0) {}

    State(const Permutation& p, const std::vector<std::string>& pa, const std::string& type, int imm)
        : perm(p), path(pa), last_op_type(type), last_op_imm(imm) {}
};


std::string perm_to_string(const Permutation& perm) {
    std::stringstream ss;
    ss << "[";
    for (size_t i = 0; i < perm.size(); ++i) {
        ss << perm[i];
        if (i < perm.size() - 1) {
            ss << ", ";
        }
    }
    ss << "]";
    return ss.str();
}

Permutation rotate_right(const Permutation& permu, int n) {
    n = n & 31;
    Permutation new_perm = permu;
    std::rotate(new_perm.rbegin(), new_perm.rbegin() + n, new_perm.rend());
    return new_perm;
}

Permutation rotate_left(const Permutation& permu, int n) {
    return rotate_right(permu, 32 - n);
}

Permutation grevi(const Permutation& permu, int imm) {
    Permutation new_perm;
    for (int i = 0; i < 32; ++i) {
        int old_idx = i;
        if ((imm & (1 << 4)) != 0) {
            old_idx = (old_idx >= 16) ? (old_idx - 16) : (old_idx + 16);
        }
        if ((imm & (1 << 3)) != 0) {
            old_idx = (old_idx % 16 >= 8) ? (old_idx - 8) : (old_idx + 8);
        }
        if ((imm & (1 << 2)) != 0) {
            old_idx = (old_idx % 8 >= 4) ? (old_idx - 4) : (old_idx + 4);
        }
        if ((imm & (1 << 1)) != 0) {
            old_idx = (old_idx % 4 >= 2) ? (old_idx - 2) : (old_idx + 2);
        }
        if ((imm & (1 << 0)) != 0) {
            old_idx = (old_idx % 2 == 1) ? (old_idx - 1) : (old_idx + 1);
        }
        new_perm[i] = permu[old_idx];
    }
    return new_perm;
}

Permutation shfli(const Permutation& permu, int imm) {
    Permutation new_perm;
    for (int i = 0; i < 32; ++i) {
        int old_idx = i;
        if ((imm & (1 << 0)) != 0) {
            int group = old_idx % 4;
            if (group == 1) {
                old_idx = old_idx + 1;
            } else if (group == 2) {
                old_idx = old_idx - 1;
            }
        }
        if ((imm & (1 << 1)) != 0) {
            int group = (old_idx % 8) / 2;
            if (group == 1) {
                old_idx = old_idx + 2;
            } else if (group == 2) {
                old_idx = old_idx - 2;
            }
        }
        if ((imm & (1 << 2)) != 0) {
            int group = (old_idx % 16) / 4;
            if (group == 1) {
                old_idx = old_idx + 4;
            } else if (group == 2) {
                old_idx = old_idx - 4;
            }
        }
        if ((imm & (1 << 3)) != 0) {
            int group = (old_idx % 32) / 8;
            if (group == 1) {
                old_idx = old_idx + 8;
            } else if (group == 2) {
                old_idx = old_idx - 8;
            }
        }
        new_perm[i] = permu[old_idx];
    }
    return new_perm;
}

Permutation unshfli(const Permutation& permu, int imm) {
    Permutation new_perm;
    for (int i = 0; i < 32; ++i) {
        int old_idx = i;
        if ((imm & (1 << 3)) != 0) {
            int group = (old_idx % 32) / 8;
            if (group == 1) {
                old_idx = old_idx + 8;
            } else if (group == 2) {
                old_idx = old_idx - 8;
            }
        }
        if ((imm & (1 << 2)) != 0) {
            int group = (old_idx % 16) / 4;
            if (group == 1) {
                old_idx = old_idx + 4;
            } else if (group == 2) {
                old_idx = old_idx - 4;
            }
        }
        if ((imm & (1 << 1)) != 0) {
            int group = (old_idx % 8) / 2;
            if (group == 1) {
                old_idx = old_idx + 2;
            } else if (group == 2) {
                old_idx = old_idx - 2;
            }
        }
        if ((imm & (1 << 0)) != 0) {
            int group = old_idx % 4;
            if (group == 1) {
                old_idx = old_idx + 1;
            } else if (group == 2) {
                old_idx = old_idx - 1;
            }
        }
        new_perm[i] = permu[old_idx];
    }
    return new_perm;
}

std::vector<std::string> find_instrs_bidirectional(const Permutation& target, int max_depth, const std::string& reg) {
    Permutation id;
    for (int i = 0; i < 32; ++i) {
        id[i] = i;
    }

    if (target == id) {
        return {};
    }

    std::vector<OperationInfo> ops;
    for (int i = 1; i < 32; ++i) {
        ops.emplace_back(
            "rori " + reg + ", " + reg + ", " + std::to_string(i),
            "rori", i,
            [i](const Permutation& p) { return rotate_right(p, i); },
            [i](const Permutation& p) { return rotate_left(p, i); }
        );
    }
    for (int imm = 1; imm < 16; ++imm) {
        std::stringstream ss_hex;
        ss_hex << std::hex << imm;
        std::string imm_str = "0x" + ss_hex.str();

        ops.emplace_back(
            "shfli " + reg + ", " + reg + ", " + imm_str,
            "shfli", imm,
            [imm](const Permutation& p) { return shfli(p, imm); },
            [imm](const Permutation& p) { return unshfli(p, imm); }
        );
        ops.emplace_back(
            "unshfli " + reg + ", " + reg + ", " + imm_str,
            "unshfli", imm,
            [imm](const Permutation& p) { return unshfli(p, imm); },
            [imm](const Permutation& p) { return shfli(p, imm); }
        );
    }
    for (int imm = 1; imm < 32; ++imm) {
        std::stringstream ss_hex;
        ss_hex << std::hex << imm;
        std::string imm_str = "0x" + ss_hex.str();

        ops.emplace_back(
            "grevi " + reg + ", " + reg + ", " + imm_str,
            "grevi", imm,
            [imm](const Permutation& p) { return grevi(p, imm); },
            [imm](const Permutation& p) { return grevi(p, imm); }
        );
    }

    std::unordered_map<Permutation, std::vector<std::string>, PermutationHasher> f_vis;
    std::unordered_map<Permutation, std::vector<std::string>, PermutationHasher> b_vis;

    std::deque<State> f_q;
    std::deque<State> b_q;

    f_q.push_back({id, {}});
    f_vis[id] = {};

    b_q.push_back({target, {}});
    b_vis[target] = {};

    for (int depth = 0; depth <= max_depth; ++depth) {
        size_t f_sz = f_q.size();
        for (size_t i = 0; i < f_sz; ++i) {
            State current_state = f_q.front();
            f_q.pop_front();

            if (b_vis.count(current_state.perm)) {
                std::vector<std::string> result_fwd = current_state.path;
                std::vector<std::string> result_bwd = b_vis[current_state.perm];

                std::vector<std::string> final_result;
                final_result.push_back("// FW Path:");
                final_result.insert(final_result.end(), result_fwd.begin(), result_fwd.end());

                std::reverse(result_bwd.begin(), result_bwd.end());
                final_result.push_back("// BW Path:");
                final_result.insert(final_result.end(), result_bwd.begin(), result_bwd.end());

                return final_result;
            }

            if (current_state.path.size() < max_depth) {
                for (const auto& op : ops) {
                    bool skip_op = false;
                    if (!current_state.last_op_type.empty()) {
                        if (op.type == "grevi" && current_state.last_op_type == "grevi" && op.imm == current_state.last_op_imm) {
                            skip_op = true;
                        }
                        else if ((op.type == "shfli" && current_state.last_op_type == "unshfli" && op.imm == current_state.last_op_imm) ||
                                 (op.type == "unshfli" && current_state.last_op_type == "shfli" && op.imm == current_state.last_op_imm)) {
                            skip_op = true;
                        }
                        else if (op.type == "rori" && current_state.last_op_type == "rori") {
                            if ((op.imm + current_state.last_op_imm) % 32 == 0) {
                                skip_op = true;
                            }
                        }
                    }

                    if (skip_op) {
                        continue;
                    }

                    Permutation next_perm = op.func(current_state.perm);
                    if (!f_vis.count(next_perm)) {
                        std::vector<std::string> next_path = current_state.path;
                        next_path.push_back(op.name);
                        f_q.push_back(State(next_perm, next_path, op.type, op.imm));
                        f_vis[next_perm] = next_path;
                    }
                }
            }
        }

        size_t b_sz = b_q.size();
        for (size_t i = 0; i < b_sz; ++i) {
            State current_state = b_q.front();
            b_q.pop_front();

            if (f_vis.count(current_state.perm)) {
                std::vector<std::string> result_fwd = f_vis[current_state.perm];
                std::vector<std::string> result_bwd = current_state.path;

                std::vector<std::string> final_result;
                final_result.push_back("// --- Forward Path (Length: " + std::to_string(result_fwd.size()) + ") ---");
                final_result.insert(final_result.end(), result_fwd.begin(), result_fwd.end());

                std::reverse(result_bwd.begin(), result_bwd.end());
                final_result.push_back("// --- Backward Path (Length: " + std::to_string(result_bwd.size()) + ", Reversed) ---");
                final_result.insert(final_result.end(), result_bwd.begin(), result_bwd.end());

                return final_result;
            }

            if (current_state.path.size() < max_depth) {
                for (const auto& op : ops) {
                    bool skip_op = false;
                    if (!current_state.last_op_type.empty()) {
                        if (op.type == "grevi" && current_state.last_op_type == "grevi" && op.imm == current_state.last_op_imm) {
                            skip_op = true;
                        }
                        else if ((op.type == "shfli" && current_state.last_op_type == "unshfli" && op.imm == current_state.last_op_imm) ||
                                 (op.type == "unshfli" && current_state.last_op_type == "shfli" && op.imm == current_state.last_op_imm)) {
                            skip_op = true;
                        }
                        else if (op.type == "rori" && current_state.last_op_type == "rori") {
                            if ((op.imm + current_state.last_op_imm) % 32 == 0) {
                                skip_op = true;
                            }
                        }
                    }

                    if (skip_op) {
                        continue;
                    }

                    Permutation next_perm = op.reverse_func(current_state.perm);
                    if (!b_vis.count(next_perm)) {
                        std::vector<std::string> next_path = current_state.path;
                        next_path.push_back(op.name);
                        b_q.push_back(State(next_perm, next_path, op.type, op.imm));
                        b_vis[next_perm] = next_path;
                    }
                }
            }
        }
    }

    return {};
}

int main() {
    Permutation target;
    for (int i = 0; i < 32; ++i) {
        target[i] = i;
    }

    // x <=> y swap
    int t1 = target[1];
    target[1] = target[4];
    target[4] = t1;

    /*int t2 = target[1];
    target[1] = target[4];
    target[4] = t2;*/

    std::cout << "Target permutation: " << perm_to_string(target) << std::endl;

    auto start = std::chrono::high_resolution_clock::now();
    std::vector<std::string> instrs = find_instrs_bidirectional(target, 8, "t0");
    auto end = std::chrono::high_resolution_clock::now();
    auto dur = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);

    if (!instrs.empty()) {
        std::cout << "Found solution with " << instrs.size() << " instrs:" << std::endl;
        for (const auto& inst : instrs) {
            std::cout << inst << std::endl;
        }
        Permutation id_check;
        for (int i = 0; i < 32; ++i) {
            id_check[i] = i;
        }

        Permutation res_perm = id_check;
        for (const auto& instr_str : instrs) {
            if (instr_str.rfind("//", 0) == 0) {
                continue;
            }
            std::stringstream ss(instr_str);
            std::string opcode, reg1, reg2, imm_str;
            ss >> opcode >> reg1 >> reg2 >> imm_str;
            int imm = std::stoi(imm_str, nullptr, 0);

            if (opcode == "rori") { res_perm = rotate_right(res_perm, imm); }
            else if (opcode == "grevi") { res_perm = grevi(res_perm, imm); }
            else if (opcode == "shfli") { res_perm = shfli(res_perm, imm); }
            else if (opcode == "unshfli") { res_perm = unshfli(res_perm, imm); }
        }

        if (res_perm == target) {
            std::cout << "\nVerification: Sequence correctly transforms id to target." << std::endl;
            std::cout << "Expected: " << perm_to_string(target) << std::endl;
            std::cout << "Result:   " << perm_to_string(res_perm) << std::endl;
        } else {
            std::cout << "\nVerification: Sequence found, but does NOT match target." << std::endl;
            std::cout << "Expected: " << perm_to_string(target) << std::endl;
            std::cout << "Result:   " << perm_to_string(res_perm) << std::endl;
        }
    } else { std::cout << "No solution found within the depth limit." << std::endl; }
    std::cout << "Search completed in " << dur.count() << " ms" << std::endl;
    return 0;
}

// Compiled with: g++ -std=c++17 -O3 -march=native perm_solver_id_evasion.cpp -o psie
// Run: ./psie