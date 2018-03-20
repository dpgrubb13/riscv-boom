package boom

import Chisel._
import freechips.rocketchip.config.{Parameters, Field}

case class Lab3Parameters(
  enabled: Boolean = true,
  history_length: Int = 1,
  info_size: Int = 0)

case object Lab3Key extends Field[Lab3Parameters]

class Lab3BrPredictor(
    fetch_width: Int,
    history_length: Int)(implicit p: Parameters)
      extends BrPredictor(fetch_width, history_length)(p)
{
  
  require (coreInstBytes == 4)
  require (fetch_width == 1)

  //instantiate tables here
  val global_counters = Reg(init = Vec(Seq.fill(512) { UInt("b00", width = 2) }))
  val local_counters = Reg(init = Vec(Seq.fill(128) { UInt("b00", width = 2) }))
  val local_indexer = Reg(init = Vec(Seq.fill(128) { UInt("b000000000", width = 7) }))
  val arbiter = Reg(init = Vec(Seq.fill(512) { UInt("b00", width = 2) }))

  //pause prediction
  val stall = !io.resp.ready

  //get pc
  val s1_pc = io.req_pc
  val s1_r_idx = s1_pc >> UInt(log2Ceil(coreInstBytes))
  val s1_local_idx = local_indexer(s1_r_idx)

  //Global Prediction
  val g_pred = RegNext(global_counters(this.ghistory), !stall)
  
  //Local Prediction
  val l_pred = RegNext(local_counters(s1_local_idx), !stall)

  //Arbiter Prediction
  val choice = RegEnable(arbiter(s1_r_idx), !stall)   

  val s2_history = RegEnable(this.ghistory, !stall)
  val s2_r_idx = RegEnable(s1_r_idx(8, 0), !stall)
  val s2_local_idx = RegEnable(s1_local_idx, !stall)

  //keep sending predictions as long as not disabled
  io.resp.valid := !this.disable_bpd
  //send prediction choosing between global and local using arbiter
  io.resp.bits.takens := Mux(choice(1), g_pred(1), l_pred(1))
  //tell the pipeline to save information for commit
  io.resp.bits.info := RegNext(Cat(g_pred(1), l_pred(1),
      s2_history, s2_r_idx, s2_local_idx))  

  //on commit, get the information and whether the branch was actually taken
  val commit_s1_en = this.commit.valid
  val commit_s1_l_index_idx = this.commit.bits.info.info(13, 7)
  val commit_s1_l_counter_idx = this.commit.bits.info.info(6, 0)
  val commit_s1_g_idx = this.commit.bits.info.info(24, 16)
  val commit_s1_a_idx = this.commit.bits.info.info(15, 7)
  val commit_s1_taken = this.commit.bits.ctrl.taken(0)
  val commit_s1_g_pred = this.commit.bits.info.info(26)
  val commit_s1_l_pred = this.commit.bits.info.info(25)

  //index into table to get previous state  
  val commit_s2_l_index_idx = RegEnable(commit_s1_l_index_idx, commit_s1_en)
  val commit_s2_l_counter_idx = RegEnable(commit_s1_l_counter_idx, commit_s1_en)
  val commit_s2_l_count = RegEnable(local_counters(commit_s1_l_counter_idx), commit_s1_en)
  val commit_s2_g_idx = RegEnable(commit_s1_g_idx, commit_s1_en)
  val commit_s2_g_count = RegEnable(global_counters(commit_s1_g_idx), commit_s1_en)
  val commit_s2_a_idx = RegEnable(commit_s1_a_idx, commit_s1_en)
  val commit_s2_a_count = RegEnable(arbiter(commit_s1_a_idx), commit_s1_en)
  val commit_s2_taken = RegEnable(commit_s1_taken, commit_s1_en)
  val commit_s2_g_pred = RegEnable(commit_s1_g_pred, commit_s1_en)
  val commit_s2_l_pred = RegEnable(commit_s1_l_pred, commit_s1_en)
  val commit_s2_en = RegNext(commit_s1_en)

  //calculate updated counter values
  val commit_s2_l_update = Mux(commit_s2_taken, 
    Mux(commit_s2_l_count === "b11".U, commit_s2_l_count, commit_s2_l_count + 1.U),
    Mux(commit_s2_l_count === "b00".U, commit_s2_l_count, commit_s2_l_count - 1.U))

  val commit_s2_g_update = Mux(commit_s2_taken, 
    Mux(commit_s2_g_count === "b11".U, commit_s2_g_count, commit_s2_g_count + 1.U),
    Mux(commit_s2_g_count === "b00".U, commit_s2_g_count, commit_s2_g_count - 1.U))

  val pred_same = commit_s2_g_pred === commit_s2_l_pred
  val commit_s2_a_update = Mux(pred_same,
    commit_s2_a_count, //if the predictions are the same, keep the arbiter table constant
    Mux(commit_s2_g_pred === commit_s2_taken, 
      Mux(commit_s2_a_count === "b11".U, commit_s2_a_count, commit_s2_a_count + 1.U),
      Mux(commit_s2_a_count === "b00".U, commit_s2_a_count, commit_s2_a_count - 1.U)))

  //write back to table
  when (commit_s2_en) { 
    local_counters(commit_s2_l_counter_idx) := commit_s2_l_update 
    //local_indexer(commit_s2_l_index_idx) := commit_s2_l_index_update
    val old = local_indexer(commit_s2_l_index_idx)
    local_indexer(commit_s2_l_index_idx) := Cat(old, commit_s2_taken)
    global_counters(commit_s2_g_idx) := commit_s2_g_update
    arbiter(commit_s2_a_idx) := commit_s2_a_update 
  }


}

