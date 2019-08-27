%%%
%%% High Dynamic Range (HDR) Histogram for Erlang
%%%
%%% This implementation is based on the Elixir version found at
%%% https://github.com/2nd/histogrex/ with adjustments based on
%%% https://github.com/HdrHistogram/hdr_histogram_erl.
%%%

%%%
%%% The MIT License (MIT)
%%% 
%%% Copyright (c) 2017 Second Spectrum
%%% 
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.

-module(xprof_core_hist).

-export([new/2,
         new/3,
         new_concurrent/4,
         record/2,
         record_many/3,
         reset/1,
         delete/1,

         total_count/1,
         max/1,
         min/1,
         mean/1,
         percentile/2,

         stats/1
        ]).

%% compatibility API with hdr_histogram_erl for testing
-export([open/2,
         open/3,
         close/1,
         get_total_count/1,
         same/3
        ]).

%% API with compile time configurable backend
-export([hdr_new/2,
         hdr_record/2,
         hdr_reset/1,
         hdr_stats/1
        ]).

-define(TABLE, ?MODULE).

-define(TOTAL_COUNT_INDEX, 2).

-record(hist,
        {table,
         %% field names from elixir
         name,
         bucket_count,
         counts_length,
         unit_magnitude,
         sub_bucket_mask,
         sub_bucket_count,
         sub_bucket_half_count,
         sub_bucket_half_count_magnitude

         %% additional field names from C
         , min %% lowest_trackable_value,
         , max %% highest_trackable_value,
         , precision %% significant_figures
        }).

%%
%% API with compile time configurable backend
%%

-ifdef(XPROF_ERL_HIST).

hdr_new(Max, Prec) ->
    new(Max, Prec).

hdr_record(HistRef, Value) ->
    record(HistRef, Value).

hdr_reset(HistRef) ->
    reset(HistRef).

hdr_stats(HistRef) ->
    stats(HistRef).

-else.

hdr_new(Max, Prec) ->
    hdr_histogram:open(Max, Prec).

hdr_record(HistRef, Value) ->
    hdr_histogram:record(HistRef, Value).

hdr_reset(HistRef) ->
    hdr_histogram:reset(HistRef).

hdr_stats(HistRef) ->
    [{count, hdr_histogram:get_total_count(HistRef)},
     {min,   hdr_histogram:min(HistRef)},
     {mean,  hdr_histogram:mean(HistRef)},
     {max,   hdr_histogram:max(HistRef)},
     {p50,   hdr_histogram:percentile(HistRef, 50.0)},
     {p75,   hdr_histogram:percentile(HistRef, 75.0)},
     {p90,   hdr_histogram:percentile(HistRef, 90.0)},
     {p99,   hdr_histogram:percentile(HistRef, 99.0)}
    ].

-endif.

%%
%% Aliases from hdr_histogram NIF API
%%

open(Max, Prec) ->
    new(1, Max, Prec).

open(Name, Max, Prec) ->
    new_concurrent(Name, 1, Max, Prec).

close(H) ->
    delete(H).

get_total_count(H) ->
    total_count(H).

same(H, A, B) ->
    ets:first(H#hist.table), %% badarg if H was deleted (table does not exist)
    lowest_equivalent_value(H, A) =:= lowest_equivalent_value(H, B).

%%
%% Primary API
%%

new(Max, Precision) ->
    new(1, Max, Precision).

new(Min, Max, Precision) ->
    Tid = storage_new(),
    do_new(Tid, Min, Max, Precision).

new_concurrent(Name, Min, Max, Precision) ->
    Tid = storage_new_concurrent(Name),
    do_new(Tid, Min, Max, Precision).

do_new(Table, Min, Max, Precision)
  when Min > 0 andalso Max > Min
       andalso 1 =< Precision andalso Precision =< 5 ->

    LargestValueWithSingleUnitResolution = 2 * math:pow(10, Precision),
    SubBucketCountMagnitude = int_ceil(math:log2(LargestValueWithSingleUnitResolution)),

    SubBucketHalfCountMagnitude =
        case SubBucketCountMagnitude < 1 of
            true -> 1;
            false -> SubBucketCountMagnitude - 1
        end,

    UnitMagnitude =
        case int_floor(math:log2(Min)) of
            N when N < 0 -> 0;
            N -> N
        end,

    SubBucketCount = round(math:pow(2, SubBucketHalfCountMagnitude + 1)),
    SubBucketHalfCount = round(SubBucketCount / 2),
    SubBucketMask = (SubBucketCount - 1) bsl UnitMagnitude,

    BucketCount = calculate_bucket_count(SubBucketCount bsl UnitMagnitude, Max, 1),
    CountsLength = round((BucketCount + 1) * (SubBucketCount / 2)),

    H = #hist{
           table = Table,
           name = hist_key,
           bucket_count = BucketCount,
           counts_length = CountsLength,
           unit_magnitude = UnitMagnitude,
           sub_bucket_mask = SubBucketMask,
           sub_bucket_count = SubBucketCount,
           sub_bucket_half_count = SubBucketHalfCount,
           sub_bucket_half_count_magnitude = SubBucketHalfCountMagnitude,

           min = Min,
           max = Max,
           precision = Precision
          },
    reset(H),
    {ok, H}.

record(H, Value) when is_integer(Value) ->
    do_record(H, Value, 1).

record_many(H, Value, N) when is_integer(Value), is_integer(N), N > 0 ->
    do_record(H, Value, N).

do_record(H, Value, N) ->
    Index = get_value_index(H, Value),
    case H#hist.max < Value orelse
        Index < 0 orelse H#hist.counts_length =< Index of
        true ->
            {error, value_out_of_range};
        false ->
            storage_record(H, Index, N)
    end.

reset(H) ->
    storage_reset(H).

delete(H) ->
    storage_delete(H).

%% @doc Get the total number of recorded values. This is O(1)
-spec total_count(#hist{}) -> non_neg_integer().
total_count(H) ->
    Counts = storage_get_counts(H),
    element(?TOTAL_COUNT_INDEX, Counts).

max(H) ->
    hd(do_get_multi_value(iterator(H), [max])).

min(H) ->
    hd(do_get_multi_value(iterator(H), [min])).

mean(H) ->
    do_mean(iterator(H)).

-spec percentile(#hist{}, float()) -> float().
percentile(H, Q) when Q > 0 andalso Q =< 100 ->
    hd(do_get_multi_value(iterator(H), [{percentile, Q}])).

stats(H) ->
    It = iterator(H),
    [Min, P50, P75, P90, P99, Max] =
        do_get_multi_value(
          It,
          [min,
           {percentile, 50.0},
           {percentile, 75.0},
           {percentile, 90.0},
           {percentile, 99.0},
           max]),
    [{count, do_total_count(It)},
     {min, Min},
     {mean, do_mean(It)},
     {max, Max},
     {p50, P50},
     {p75, P75},
     {p90, P90},
     {p99, P99}
    ].

%%
%% Storage
%%

storage_new() ->
    ets:new(?MODULE, [set, private]).

storage_new_concurrent(Name) ->
    ets:new(Name, [set, public, {write_concurrency, true}]).

storage_record(H, Index, N) ->
    ets:update_counter(H#hist.table, H#hist.name,
                       [{?TOTAL_COUNT_INDEX, N},
                        {Index + ?TOTAL_COUNT_INDEX + 1, N}]),
    ok.

storage_get_counts(H) ->
    case ets:lookup(H#hist.table, H#hist.name) of
        [] ->
            throw(data_missing_from_ets);
        [Counts] ->
            Counts
    end.

storage_reset(H) ->
    ets:insert(H#hist.table, create_row(H#hist.name, H#hist.counts_length)),
    ok.

storage_delete(H) ->
    ets:delete(H#hist.table),
    ok.

create_row(Name, Count) ->
    %% counters come after name and total_count that are stored at the start
    erlang:make_tuple(?TOTAL_COUNT_INDEX + Count, 0, [{1, Name}]).

%%
%% Calculations
%%

round_to_significant_figures(0, _) ->
    0;
round_to_significant_figures(V, Prec) ->
    Factor = math:pow(10.0, Prec - int_ceil(math:log10(abs(V)))),
    round(V * Factor) / Factor.

calculate_bucket_count(SmallestUntrackableValue, Max, BucketCount) ->
    case SmallestUntrackableValue < Max of
        false -> BucketCount;
        true -> calculate_bucket_count((SmallestUntrackableValue bsl 1),
                                       Max, BucketCount + 1)
    end.

get_value_index(H, Value) ->
    {Bucket, Sub} = get_bucket_indexes(H, Value),
    get_count_index(H, Bucket, Sub).

get_bucket_indexes(H, Value) ->
    Ceiling = bit_length((Value bor H#hist.sub_bucket_mask), 0),
    BucketIndex = Ceiling - H#hist.unit_magnitude - (H#hist.sub_bucket_half_count_magnitude + 1),

    SubBucketIndex = Value bsr (BucketIndex + H#hist.unit_magnitude),
    {BucketIndex, SubBucketIndex}.

get_bucket_indexes_from_index(H, Index) when Index < H#hist.sub_bucket_half_count ->
    {0, Index};
get_bucket_indexes_from_index(H, Index) ->
    %%Magn = H#hist.sub_bucket_half_count_magnitude,
    BucketIndex = (Index bsr H#hist.sub_bucket_half_count_magnitude) - 1,
    SubBucketIndex = (Index + H#hist.sub_bucket_half_count)
        - ((BucketIndex + 1) bsl H#hist.sub_bucket_half_count_magnitude),
    {BucketIndex, SubBucketIndex}.

bit_length(Value, N) when Value >= 32768 ->
    bit_length((Value bsr 16), N + 16);
bit_length(Value, N) ->
    {Value2, N2} = case Value >= 128 of
                       true -> {(Value bsr 8), N + 8};
                       false -> {Value, N}
                   end,

    {Value3, N3} = case Value2 >= 8 of
                       true -> {(Value2 bsr 4), N2 + 4};
                       false -> {Value2, N2}
                   end,

    {Value4, N4} = case Value3 >= 2 of
                       true -> {(Value3 bsr 2), N3 + 2};
                       false -> {Value3, N3}
                   end,

    case Value4 =:= 1 of
        true -> N4 + 1;
        false -> N4
    end.

get_count_index(H, BucketIndex, SubBucketIndex) ->
    BucketBaseIndex =
        (BucketIndex + 1) bsl H#hist.sub_bucket_half_count_magnitude,
    OffsetInBucket = SubBucketIndex - H#hist.sub_bucket_half_count,
    BucketBaseIndex + OffsetInBucket.

value_from_index(H, BucketIndex, SubBucketIndex) ->
    SubBucketIndex bsl (BucketIndex + H#hist.unit_magnitude).

highest_equivalent_value(H, BucketIndex, SubBucketIndex) ->
    next_non_equivalent_value(H, BucketIndex, SubBucketIndex) - 1.

lowest_equivalent_value(H, Value) ->
    {BucketIndex, SubBucketIndex} = get_bucket_indexes(H, Value),
    lowest_equivalent_value(H, BucketIndex, SubBucketIndex).

lowest_equivalent_value(H, BucketIndex, SubBucketIndex) ->
    value_from_index(H, BucketIndex, SubBucketIndex).

next_non_equivalent_value(H, BucketIndex, SubBucketIndex) ->
    lowest_equivalent_value(H, BucketIndex, SubBucketIndex)
        + size_of_equivalent_value_range(H, BucketIndex, SubBucketIndex).

median_equivalent_value(H, BucketIndex, SubBucketIndex) ->
    lowest_equivalent_value(H, BucketIndex, SubBucketIndex)
        + (size_of_equivalent_value_range(H, BucketIndex, SubBucketIndex) bsr 1).

size_of_equivalent_value_range(H, BucketIndex, SubBucketIndex) ->
    AdjustedBucketIndex =
        case SubBucketIndex >= H#hist.sub_bucket_count of
            true -> BucketIndex + 1;
            false -> BucketIndex
        end,
    1 bsl (H#hist.unit_magnitude + AdjustedBucketIndex).

%%
%% Iteration
%%

-record(it,
        {h :: #hist{},
         total_count,
         counts
        }).

iterator(H) ->
    Counts = storage_get_counts(H),
    #it{h = H,
        counts = Counts,
        total_count = element(?TOTAL_COUNT_INDEX, Counts)}.

do_total_count(It) ->
    It#it.total_count.

do_mean(It) ->
    case It#it.total_count of
        0 -> 0;
        TotalCount ->
            TotalSum = do_mean_loop(It, 0, 0, 0),
            Mean = TotalSum / TotalCount,

            %% the NIF does this rounding on the value returned from c code
            round_to_significant_figures(Mean, It#it.h#hist.precision)
    end.

do_mean_loop(It, Index, CountToIndex, Total0) ->
    case CountToIndex >= It#it.total_count of
        true -> Total0;
        false ->
            CountAtIndex = count_at_index(It, Index),
            Total =
                case CountAtIndex of
                    0 -> Total0;
                    N ->
                        {BucketIndex, SubBucketIndex} =
                            get_bucket_indexes_from_index(It#it.h, Index),
                        Total0 + N * median_equivalent_value(
                                       It#it.h,
                                       BucketIndex,
                                       SubBucketIndex)
                end,
            do_mean_loop(It, Index + 1, CountToIndex + CountAtIndex, Total)
    end.

do_get_multi_value(#it{total_count = 0}, QList) ->
    [0 || _ <- QList];
do_get_multi_value(It, QList) ->
    PreparedQList =
        [case Item of
             max ->
                 {max, It#it.total_count};
             min ->
                 {min, 1};
             {percentile, Q} ->
                 CountAtPercentile = round(Q / 100 * It#it.total_count),
                 {percentile, CountAtPercentile}
         end
         || Item <- QList],
    CountAtIndex = count_at_index(It, 0),
    do_multi_loop(It, 0, CountAtIndex, PreparedQList, []).

do_multi_loop(It, Index, CountToIndex, [{Tag, CountAtPercentile}|Multi], Res) ->
    do_multi_loop(It, Index, CountToIndex, Tag, CountAtPercentile, Multi, Res);
do_multi_loop(_, _, _, [], Res) ->
    lists:reverse(Res).

do_multi_loop(It, Index, CountToIndex, Tag, CountAtPercentile, Multi, Res) ->
    case CountToIndex >= CountAtPercentile of
        true ->
            do_multi_loop(It, Index, CountToIndex, Multi,
                          [get_value_from_index(It#it.h, Tag, Index)|Res]);
        false ->
            NextIndex = Index + 1,
            CountAtNextIndex = count_at_index(It, NextIndex),
            CountToNextIndex = CountToIndex + CountAtNextIndex,
            do_multi_loop(It, NextIndex, CountToNextIndex, Tag, CountAtPercentile, Multi, Res)
    end.

get_value_from_index(H, max, MaxIndex) ->
    {MaxBucketIndex, MaxSubBucketIndex} =
        get_bucket_indexes_from_index(H, MaxIndex),
    %% The NIF uses an old version of the c code which calls lowest.
    %% In newer version of HdrHistogram_c hdr_max was refactorred and
    %% besides other changes it uses highest.

    %%highest_equivalent_value(It#it.h, MaxValue).
    lowest_equivalent_value(H, MaxBucketIndex, MaxSubBucketIndex);
get_value_from_index(H, min, MinIndex) ->
    {MinBucketIndex, MinSubBucketIndex} =
        get_bucket_indexes_from_index(H, MinIndex),
    lowest_equivalent_value(H, MinBucketIndex, MinSubBucketIndex);
get_value_from_index(H, percentile, Index) ->
    {BucketIndex, SubBucketIndex} =
        get_bucket_indexes_from_index(H, Index),
    V = highest_equivalent_value(H, BucketIndex, SubBucketIndex),
    %% the NIF does this rounding on the value returned from c code
    round_to_significant_figures(V, H#hist.precision).

count_at_index(It, Index) ->
    %% 1 is the name
    %% 2 is the total_count
    %% the real count buckets start at 3
    %% Index is zero based
    element(Index + ?TOTAL_COUNT_INDEX + 1, It#it.counts).

%% ceil/1 and floor/1 were introduced in OTP 20
-ifdef(ceil_floor).

int_ceil(F) ->
    erlang:ceil(F).

int_floor(F) ->
    erlang:floor(F).

-else.

int_ceil(F) ->
    R = round(F),
    if R < F -> R + 1;
       true -> R
    end.

int_floor(F) ->
    R = round(F),
    if R > F -> R - 1;
       true -> R
    end.

-endif.
