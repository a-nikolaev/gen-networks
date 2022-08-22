
execs = growth fd_dist ed_dist fs_dist fs_cond_dist fd_fs_pairs lengths stats graph_growth sc_cc q_structure q_structure2 \
				fd_correlation show_fd_correlation fd_corr_more lrgst_concomp subsume assort_knn walk \

all: $(execs)

growth: common.ml memo.ml sc.ml virt_graph.ml gamma.ml metric.ml evolve.ml undergrowth.ml asg.ml opt.ml union_find.ml concomp.ml growth.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

fd_dist: common.ml memo.ml sc.ml opt.ml fd_dist.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

ed_dist: common.ml memo.ml sc.ml opt.ml ed_dist.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

fs_dist: common.ml memo.ml sc.ml opt.ml fs_dist.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

fs_cond_dist: common.ml memo.ml sc.ml opt.ml fs_cond_dist.ml
	ocamlfind ocamlopt -package str -package unix -linkpkg -o $@ $^

fd_fs_pairs: common.ml memo.ml sc.ml opt.ml fd_fs_pairs.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

lengths: common.ml memo.ml sc.ml opt.ml asg.ml lengths.ml 
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

stats: common.ml memo.ml sc.ml gamma.ml metric.ml opt.ml stats.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

render_tree: memo.ml sc.ml virt_graph.ml render_tree.ml
	ocamlfind ocamlopt -package cairo2 -linkpkg -o $@ $^

graph_growth: common.ml memo.ml sc.ml gr.ml graph_growth.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

sc_cc: common.ml memo.ml sc.ml gr.ml opt.ml sc_cc.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

q_structure: common.ml memo.ml sc.ml union_find.ml opt.ml q_structure.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

q_structure2: common.ml memo.ml sc.ml union_find.ml opt.ml q_structure2.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

lrgst_concomp: common.ml memo.ml sc.ml union_find.ml concomp.ml opt.ml lrgst_concomp.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

fd_correlation: common.ml memo.ml sc.ml opt.ml fd_correlation.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

show_fd_correlation: common.ml memo.ml sc.ml opt.ml show_fd_correlation.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

fd_corr_more: common.ml memo.ml sc.ml opt.ml fd_corr_more.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

subsume: common.ml memo.ml sc.ml subsume.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

assort_knn: common.ml memo.ml sc.ml asg.ml opt.ml assort_knn.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

walk: common.ml memo.ml sc.ml opt.ml walk.ml
	ocamlfind ocamlopt -package str -linkpkg -o $@ $^

virt_graph_degrees: memo.ml sc.ml virt_graph.ml virt_graph_degrees.ml
	ocamlfind ocamlopt -package cairo2 -linkpkg -o $@ $^

clean:
	- rm -rf $(execs) *.o *.cmx *.cmi
