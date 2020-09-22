let sched_req_template_data_unit_matches_sched_req_data_unit
    (sched_req_template_data_unit : Task.sched_req_template_data_unit)
    (sched_req_data_unit : Sched_req.sched_req_data_unit) : bool =
  match (sched_req_template_data_unit, sched_req_data_unit) with
  | ( Sched_req_data_unit_skeleton.Fixed
        { task_seg_related_data = size1; start = start1 },
      Sched_req_data_unit_skeleton.Fixed
        { task_seg_related_data = _id, size2; start = start2 } ) ->
    size1 = size2 && start1 = start2
  | ( Sched_req_data_unit_skeleton.Shift
        { task_seg_related_data_list = sizes1; time_slots = time_slots1; _ },
      Sched_req_data_unit_skeleton.Shift
        { task_seg_related_data_list = task_segs; time_slots = time_slots2; _ }
    )
  | ( Sched_req_data_unit_skeleton.Time_share
        { task_seg_related_data_list = sizes1; time_slots = time_slots1; _ },
      Sched_req_data_unit_skeleton.Time_share
        { task_seg_related_data_list = task_segs; time_slots = time_slots2; _ }
    ) ->
    let sizes2 = List.map (fun (_, size) -> size) task_segs in
    List.sort_uniq compare sizes1 = List.sort_uniq compare sizes2
    && Time_slots.equal time_slots1 time_slots2
  | ( Sched_req_data_unit_skeleton.Split_and_shift
        { task_seg_related_data = size1; time_slots = time_slots1; _ },
      Sched_req_data_unit_skeleton.Split_and_shift
        { task_seg_related_data = _id, size2; time_slots = time_slots2; _ } ) ->
    size1 = size2 && Time_slots.equal time_slots1 time_slots2
  | ( Sched_req_data_unit_skeleton.Split_even
        {
          task_seg_related_data = size1;
          time_slots = time_slots1;
          buckets = buckets1;
        },
      Sched_req_data_unit_skeleton.Split_even
        {
          task_seg_related_data = _id, size2;
          time_slots = time_slots2;
          buckets = buckets2;
        } ) ->
    size1 = size2
    && Time_slots.equal time_slots1 time_slots2
    && Time_slots.equal buckets1 buckets2
  | ( Push_toward
        {
          task_seg_related_data = size1;
          target = target1;
          time_slots = time_slots1;
        },
      Push_toward
        {
          task_seg_related_data = _id, size2;
          target = target2;
          time_slots = time_slots2;
        } ) ->
    target1 = target2
    && size1 = size2
    && Time_slots.equal time_slots1 time_slots2
  | _ -> false

let sched_req_template_matches_sched_req_data
    (sched_req_template : Task.sched_req_template)
    (sched_req_data : Sched_req.sched_req_data) : bool =
  List.for_all2
    (fun sched_req_template_data_unit sched_req_data_unit ->
       sched_req_template_data_unit_matches_sched_req_data_unit
         sched_req_template_data_unit sched_req_data_unit)
    sched_req_template sched_req_data

let sched_req_template_matches_sched_req
    (sched_req_template : Task.sched_req_template)
    ((_sched_req_id, sched_req_data) : Sched_req.sched_req) : bool =
  sched_req_template_matches_sched_req_data sched_req_template sched_req_data

let sched_req_template_data_unit_matches_sched_req_record_data_unit
    (sched_req_template_data_unit : Task.sched_req_template_data_unit)
    (sched_req_record_data_unit : Sched_req.sched_req_record_data_unit) : bool =
  match (sched_req_template_data_unit, sched_req_record_data_unit) with
  | ( Sched_req_data_unit_skeleton.Fixed
        { task_seg_related_data = size1; start = start1 },
      Sched_req_data_unit_skeleton.Fixed
        { task_seg_related_data = _id, size2; start = start2 } ) ->
    size1 = size2 && start1 = start2
  | ( Sched_req_data_unit_skeleton.Shift
        { task_seg_related_data_list = sizes1; time_slots = time_slots1 },
      Sched_req_data_unit_skeleton.Shift
        { task_seg_related_data_list = task_segs; time_slots = time_slots2 } )
  | ( Sched_req_data_unit_skeleton.Time_share
        { task_seg_related_data_list = sizes1; time_slots = time_slots1 },
      Sched_req_data_unit_skeleton.Time_share
        { task_seg_related_data_list = task_segs; time_slots = time_slots2 } )
    ->
    let sizes2 = List.map (fun (_, size) -> size) task_segs in
    List.sort_uniq compare sizes1 = List.sort_uniq compare sizes2
    && Time_slots.equal time_slots1 time_slots2
  | ( Sched_req_data_unit_skeleton.Split_and_shift
        { task_seg_related_data = size1; time_slots = time_slots1 },
      Sched_req_data_unit_skeleton.Split_and_shift
        { task_seg_related_data = _id, size2; time_slots = time_slots2 } ) ->
    size1 = size2 && Time_slots.equal time_slots1 time_slots2
  | ( Sched_req_data_unit_skeleton.Split_even
        {
          task_seg_related_data = size1;
          time_slots = time_slots1;
          buckets = buckets1;
        },
      Sched_req_data_unit_skeleton.Split_even
        {
          task_seg_related_data = _id, size2;
          time_slots = time_slots2;
          buckets = buckets2;
        } ) ->
    size1 = size2
    && Time_slots.equal time_slots1 time_slots2
    && Time_slots.equal buckets1 buckets2
  | ( Push_toward
        {
          task_seg_related_data = size1;
          target = target1;
          time_slots = time_slots1;
        },
      Push_toward
        {
          task_seg_related_data = _id, size2;
          target = target2;
          time_slots = time_slots2;
        } ) ->
    target1 = target2
    && size1 = size2
    && Time_slots.equal time_slots1 time_slots2
  | _ -> false

let sched_req_template_matches_sched_req_record_data
    (sched_req_template : Task.sched_req_template)
    (sched_req_record_data : Sched_req.sched_req_record_data) : bool =
  List.for_all2
    (fun sched_req_template_data_unit sched_req_record_data_unit ->
       sched_req_template_data_unit_matches_sched_req_record_data_unit
         sched_req_template_data_unit sched_req_record_data_unit)
    sched_req_template sched_req_record_data

let sched_req_template_matches_sched_req_record
    (sched_req_template : Task.sched_req_template)
    ((_sched_req_record_id, sched_req_record_data) : Sched_req.sched_req_record)
  : bool =
  sched_req_template_matches_sched_req_record_data sched_req_template
    sched_req_record_data
