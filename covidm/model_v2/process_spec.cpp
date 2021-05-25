// changes.cpp

#include "process_spec.h"

void ProcessList::Update(vector<ProcessSpec>& ps) {
    
    // iterate through all flows, recording + id'ing sources & sinks
    for (ProcessSpec flow : ps) {
        
        // 1. define source id
        // look for the source in state names
        auto source = std::find(state_names.begin(), state_names.end(), flow.source_name);
        if (source == state_names.end()) {
            // if the source name is not yet defined, it must be in guaranteed processes
            if (processSourceMap.find(flow.source_name) == processSourceMap.end())
                throw logic_error("Unrecognized process source name " + flow.source_name);
            else
                flow.source_id = processSourceMap.at(flow.source_name);
        } else {
            // if it is defined, use the appropriate source id
            flow.source_id = (unsigned int)(source - state_names.begin());
        }
        
        // 2. define sink ids
        for (size_t i = 0; i < flow.names.size(); i++) {
            auto sink_name = flow.names[i];
            // only care about recording non-null sinks
            if (sink_name != "null") {
                auto sink = std::find(state_names.begin(), state_names.end(), sink_name);
                // if this is a newly identified sink
                if (sink == state_names.end()) {
                    state_names.push_back(sink_name);
                    unsigned int newind = state_names.size()-1;
                    // need to check reporting for this state
                    for (size_t k = 0; k < flow.report[i].size(); k++) if (flow.report[i][k] == 'p') {
                        prevalence_states.push_back(newind);
                    } else if (flow.report[i][k] == 'i') {
                        incidence_states.push_back(newind);
                    } else if (flow.report[i][k] == 'o') {
                        outcidence_states.push_back(newind);
                    } else {
                        throw runtime_error("Unrecognized process report type " + string(1, flow.report[i][k]) + ".");
                    }
                    flow.sink_ids.push_back(newind);
                } else flow.sink_ids.push_back((unsigned int)(sink - state_names.begin()));
                // (sink - begin) will either be the found location or the newly created location
                
            } else if (i != flow.names.size() - 1) {
                throw logic_error("null process sink not last in flow from " + flow.source_name);
            }
        }
        
        // 3. add process to ProcessList items
        flows.push_back(flow);
    }
    
    // 4. Update offset references
    state_count = state_names.size();
    recording_count = prevalence_states.size() + incidence_states.size() + outcidence_states.size();
    inc_offset = prevalence_states.size();
    out_offset = inc_offset + incidence_states.size();
}

