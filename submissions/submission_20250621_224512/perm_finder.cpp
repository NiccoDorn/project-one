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
#include <memory>

using Permutation = std::array<int, 32>;

struct PermutationHasher {
    size_t operator()(const Permutation& p) const {
        size_t hash = 0;
        for (int x : p) {
            hash ^= std::hash<int>()(x) + 0x9e3779b9 + (hash << 6) + (hash >> 2);
        }
        return hash;
    }
};

struct SearchNode;
using SearchNodePtr = std::shared_ptr<SearchNode>;

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

struct SearchNode {
    Permutation perm;
    SearchNodePtr parent;
    std::string op_name;
    std::string op_type;
    int op_imm;
    int depth;

    SearchNode(const Permutation& p, int d)
        : perm(p), parent(nullptr), op_name(""), op_type(""), op_imm(0), depth(d) {}

    SearchNode(const Permutation& p, SearchNodePtr par, const std::string& name, const std::string& type, int imm, int d)
        : perm(p), parent(par), op_name(name), op_type(type), op_imm(imm), depth(d) {}
};


std::string perm_to_string(const Permutation& perm) {
    std::stringstream ss;
    ss << "[";
    for (size_t i = 0; i < perm.size(); ++i) {
        ss << perm[i];
        if (i < perm.size() - 1) { ss << ", "; }
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
        if ((imm & (1 << 4)) != 0) { old_idx = (old_idx >= 16) ? (old_idx - 16) : (old_idx + 16); }
        if ((imm & (1 << 3)) != 0) { old_idx = (old_idx % 16 >= 8) ? (old_idx - 8) : (old_idx + 8); }
        if ((imm & (1 << 2)) != 0) { old_idx = (old_idx % 8 >= 4) ? (old_idx - 4) : (old_idx + 4); }
        if ((imm & (1 << 1)) != 0) { old_idx = (old_idx % 4 >= 2) ? (old_idx - 2) : (old_idx + 2); }
        if ((imm & (1 << 0)) != 0) { old_idx = (old_idx % 2 == 1) ? (old_idx - 1) : (old_idx + 1); }
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
            if (group == 1) { old_idx = old_idx + 1; }
            else if (group == 2) { old_idx = old_idx - 1; }
        }
        if ((imm & (1 << 1)) != 0) {
            int group = (old_idx % 8) / 2;
            if (group == 1) { old_idx = old_idx + 2; }
            else if (group == 2) { old_idx = old_idx - 2; }
        }
        if ((imm & (1 << 2)) != 0) {
            int group = (old_idx % 16) / 4;
            if (group == 1) { old_idx = old_idx + 4; }
            else if (group == 2) { old_idx = old_idx - 4; }
        }
        if ((imm & (1 << 3)) != 0) {
            int group = (old_idx % 32) / 8;
            if (group == 1) { old_idx = old_idx + 8; }
            else if (group == 2) { old_idx = old_idx - 8; }
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
            if (group == 1) { old_idx = old_idx + 8; }
            else if (group == 2) { old_idx = old_idx - 8; }
        }
        if ((imm & (1 << 2)) != 0) {
            int group = (old_idx % 16) / 4;
            if (group == 1) { old_idx = old_idx + 4; }
            else if (group == 2) { old_idx = old_idx - 4; }
        }
        if ((imm & (1 << 1)) != 0) {
            int group = (old_idx % 8) / 2;
            if (group == 1) { old_idx = old_idx + 2;}
            else if (group == 2) { old_idx = old_idx - 2; }
        }
        if ((imm & (1 << 0)) != 0) {
            int group = old_idx % 4;
            if (group == 1) { old_idx = old_idx + 1; }
            else if (group == 2) { old_idx = old_idx - 1; }
        }
        new_perm[i] = permu[old_idx];
    }
    return new_perm;
}

std::vector<std::string> recon_path(SearchNodePtr node) {
    std::vector<std::string> path;
    while (node && node->parent) {
        path.push_back(node->op_name);
        node = node->parent;
    }
    std::reverse(path.begin(), path.end());
    return path;
}


std::vector<std::string> find_instrs_bidirectional(const Permutation& target, int max_depth, const std::string& reg) {
    Permutation id;
    for (int i = 0; i < 32; ++i) { id[i] = i; }
    if (target == id) { return {}; }

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

    std::unordered_map<Permutation, SearchNodePtr, PermutationHasher> f_vis;
    std::unordered_map<Permutation, SearchNodePtr, PermutationHasher> b_vis;

    std::deque<SearchNodePtr> f_q;
    std::deque<SearchNodePtr> b_q;

    SearchNodePtr f_snode = std::make_shared<SearchNode>(id, 0);
    f_q.push_back(f_snode);
    f_vis[id] = f_snode;

    SearchNodePtr b_snode = std::make_shared<SearchNode>(target, 0);
    b_q.push_back(b_snode);
    b_vis[target] = b_snode;

    for (int depth = 0; depth <= max_depth; ++depth) {
        size_t f_sz = f_q.size();
        for (size_t i = 0; i < f_sz; ++i) {
            SearchNodePtr curr_node = f_q.front();
            f_q.pop_front();
            if (b_vis.count(curr_node->perm)) {
                SearchNodePtr b_node = b_vis[curr_node->perm];
                std::vector<std::string> path_fwd = recon_path(curr_node);
                std::vector<std::string> path_bwd_rev = recon_path(b_node);
                std::vector<std::string> combined = path_fwd;
                std::reverse(path_bwd_rev.begin(), path_bwd_rev.end());
                combined.insert(combined.end(), path_bwd_rev.begin(), path_bwd_rev.end());
                std::vector<std::string> finres;
                finres.insert(finres.end(), path_fwd.begin(), path_fwd.end());
                finres.insert(finres.end(), path_bwd_rev.begin(), path_bwd_rev.end());

                return combined;
            }

            if (curr_node->depth < max_depth) {
                for (const auto& op : ops) {
                    bool skip_op = false;
                    if (!curr_node->op_type.empty()) {
                        if ((op.type == "shfli" && curr_node->op_type == "unshfli" && op.imm == curr_node->op_imm) ||
                            (op.type == "unshfli" && curr_node->op_type == "shfli" && op.imm == curr_node->op_imm) ||
                            (op.type == "grevi" && curr_node->op_type == "grevi" && op.imm == curr_node->op_imm) ||
                            (op.type == "rori" && curr_node->op_type == "rori" && (op.imm + curr_node->op_imm) % 32 == 0)) {
                            skip_op = true;
                        }
                    }
                    if (skip_op) { continue; }

                    Permutation next_perm = op.func(curr_node->perm);
                    if (!f_vis.count(next_perm)) {
                        SearchNodePtr new_node = std::make_shared<SearchNode>(next_perm, curr_node, op.name, op.type, op.imm, curr_node->depth + 1);
                        f_q.push_back(new_node);
                        f_vis[next_perm] = new_node;
                    }
                }
            }
        }

        size_t b_sz = b_q.size();
        for (size_t i = 0; i < b_sz; ++i) {
            SearchNodePtr curr_node = b_q.front();
            b_q.pop_front();

            if (f_vis.count(curr_node->perm)) {
                SearchNodePtr f_node = f_vis[curr_node->perm];
                std::vector<std::string> path_fwd = recon_path(f_node);
                std::vector<std::string> path_bwd = recon_path(curr_node);

                std::vector<std::string> combined = path_fwd;
                std::reverse(path_bwd.begin(), path_bwd.end());
                combined.insert(combined.end(), path_bwd.begin(), path_bwd.end());

                std::vector<std::string> finres;
                finres.insert(finres.end(), path_fwd.begin(), path_fwd.end());
                finres.insert(finres.end(), path_bwd.begin(), path_bwd.end());

                return combined;
            }

            if (curr_node->depth < max_depth) {
                for (const auto& op : ops) {
                    bool skip_op = false;
                    if (!curr_node->op_type.empty()) {
                        if ((op.type == "shfli" && curr_node->op_type == "unshfli" && op.imm == curr_node->op_imm) ||
                            (op.type == "unshfli" && curr_node->op_type == "shfli" && op.imm == curr_node->op_imm) ||
                            (op.type == "grevi" && curr_node->op_type == "grevi" && op.imm == curr_node->op_imm) ||
                            (op.type == "rori" && curr_node->op_type == "rori" && (op.imm + curr_node->op_imm) % 32 == 0)) {
                            skip_op = true;
                        }
                    }
                    if (skip_op) { continue; }

                    Permutation next_perm = op.reverse_func(curr_node->perm);
                    if (!b_vis.count(next_perm)) {
                        SearchNodePtr new_node = std::make_shared<SearchNode>(next_perm, curr_node, op.name, op.type, op.imm, curr_node->depth + 1);
                        b_q.push_back(new_node);
                        b_vis[next_perm] = new_node;
                    }
                }
            }
        }
    }

    return {};
}

int main() {
    Permutation target;
    for (int i = 0; i < 32; ++i) { target[i] = i; }

    target[1] = 4;
    target[4] = 1;

    target[31] = 30;
    target[30] = 31;

    std::cout << "Target permutation: " << perm_to_string(target) << std::endl;
    auto start = std::chrono::high_resolution_clock::now();
    std::vector<std::string> instrs = find_instrs_bidirectional(target, 5, "t0");
    auto end = std::chrono::high_resolution_clock::now();
    auto dur = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);

    if (!instrs.empty()) {
        std::cout << "Found solution with " << instrs.size() << " instrs:" << std::endl;
        for (const auto& inst : instrs) {
            std::cout << inst << std::endl;
        }
        
        Permutation id_check;
        for (int i = 0; i < 32; ++i) { id_check[i] = i; }

        Permutation res_perm = id_check;
        int ith_instr = 0;
        for (const auto& instr_str : instrs) {
            ith_instr++;
            std::stringstream ss(instr_str);
            std::string opcode, reg1, reg2, imm_str;
            ss >> opcode >> reg1 >> reg2 >> imm_str;
            int imm = std::stoi(imm_str, nullptr, 0);

            if (opcode == "rori") { res_perm = rotate_right(res_perm, imm); }
            else if (opcode == "grevi") { res_perm = grevi(res_perm, imm); }
            else if (opcode == "shfli") { res_perm = shfli(res_perm, imm); }
            else if (opcode == "unshfli") { res_perm = unshfli(res_perm, imm); }
            std::cout << "\nFor " << ith_instr << ". instruction perm is transposed to: " << std::endl;
            std::cout << perm_to_string(res_perm) << std::endl;

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

// compile it with: g++ -std=c++17 -Ofast -march=native perm_finder.cpp -o perm_finder
// note: solutions are inherently different to solutions in the scala implementation CustomPermutation.scala
// since C++ has differing representations of elements for specific permutations
// but it shows the same minimal number of sequences that are found for a certain permutation
// note: Depending on your machine, you might have to reduce depth to 4 instead of 5
// limitations of this approach with depth = 5, bidirectional: 