-module(dial).

-export([run/0]).

run () ->
	DataDir = "/home/emilio/Documentos2/Doutorado/sideprojects/versioned/otp/visual_tool/bench_explained/dialyzer",
%	[] = dialyzer:run([{analysis_type, plt_build},
%                {report_mode, normal},
%                {files_rec, [DataDir ++ "/data"]},
%                {timing, true},
%                {output_plt, DataDir ++ "/the.plt"}]),

	RawWarns = dialyzer:run([
		{files_rec, [DataDir ++ "/data"]},
                {report_mode, normal},
                {init_plt, DataDir ++ "/the.plt"},
		{timing, true}]),
        ok.

