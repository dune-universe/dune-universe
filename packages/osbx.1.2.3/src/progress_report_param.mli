module Encode : sig
  val progress_report_interval : float

  module Encode_progress : sig
    val display_while_active    : Progress_report.progress_element list
    val display_on_finish       : Progress_report.progress_element list
    val display_on_finish_early : Progress_report.progress_element list
  end
end

module Decode : sig
  val progress_report_interval : float

  module Scan_progress : sig
    val display_while_active    : Progress_report.progress_element list
    val display_on_finish       : Progress_report.progress_element list
    val display_on_finish_early : Progress_report.progress_element list
  end

  module Hash_progress : sig
    val display_while_active    : Progress_report.progress_element list
    val display_on_finish       : Progress_report.progress_element list
    val display_on_finish_early : Progress_report.progress_element list
  end

  module Decode_progress : sig
    val display_while_active    : Progress_report.progress_element list
    val display_on_finish       : Progress_report.progress_element list
    val display_on_finish_early : Progress_report.progress_element list
  end
end

module Rescue : sig
  val progress_report_interval : float

  module Rescue_progress : sig
    val display_while_active    : Progress_report.progress_element list
    val display_on_finish       : Progress_report.progress_element list
    val display_on_finish_early : Progress_report.progress_element list
  end
end

module Show : sig
  val progress_report_interval : float

  module Show_progress : sig
    val display_while_active    : Progress_report.progress_element list
    val display_on_finish       : Progress_report.progress_element list
    val display_on_finish_early : Progress_report.progress_element list
  end
end
