/* * Copyright (c) 2010 - 2012 Intel Corporation.
*
* Disclaimer: The codes contained in these modules may be specific to the
* Intel Software Development Platform codenamed: Knights Ferry, and the 
* Intel product codenamed: Knights Corner, and are not backward compatible 
* with other Intel products. Additionally, Intel will NOT support the codes 
* or instruction set in future products.
*
* Intel offers no warranty of any kind regarding the code.  This code is
* licensed on an "AS IS" basis and Intel is not obligated to provide any support,
* assistance, installation, training, or other services of any kind.  Intel is 
* also not obligated to provide any updates, enhancements or extensions.  Intel 
* specifically disclaims any warranty of merchantability, non-infringement, 
* fitness for any particular purpose, and any other warranty.
*
* Further, Intel disclaims all liability of any kind, including but not
* limited to liability for infringement of any proprietary rights, relating
* to the use of the code, even if Intel is notified of the possibility of
* such liability.  Except as expressly stated in an Intel license agreement
* provided with this code and agreed upon with Intel, no license, express
* or implied, by estoppel or otherwise, to any intellectual property rights
* is granted herein.
*/
/* ************************************************************************* *\
File Name: mic_pmcommon.h
Description: contains common power management specific header defines for
			 host and card.
\* ************************************************************************* */

#if !defined(__MIC_PMCOMMON_H)
#define __MIC_PMCOMMON_H

typedef enum _PM_MESSAGE {
	PM_MESSAGE_PC3READY,
	PM_MESSAGE_HOST_ABORTPCSTATE,
	PM_MESSAGE_UOS_ABORTPC3,
	PM_MESSAGE_PC6_REQUEST,
	PM_MESSAGE_ABORT_PC6REQUEST,
	PM_MESSAGE_PC6READY,
	PM_MESSAGE_PC6_QUERY,
	PM_MESSAGE_PC6_RESPONSE,
	PM_MESSAGE_WAKEUP_NODE,
	PM_MESSAGE_NODE_READY,
	PM_MESSAGE_CONFIGURE_PCSTATES,
	PM_MESSAGE_GET_LATENCIES,
	PM_MESSAGE_LATENCY_RESPONSE,
	PM_MESSAGE_POLICY_CHANGE,
	PM_MESSAGE_POWER_OFF,
	PM_MESSAGE_TEST,
	PM_MESSAGE_MAX,
} PM_MESSAGE;

typedef enum _PM_IDLE_STATE {
	PM_IDLE_STATE_C0,
	PM_IDLE_STATE_AUTO_C3,
	PM_IDLE_STATE_DEEP_C3,
	PM_IDLE_STATE_C6,
	PM_IDLE_STATE_MAX,
} PM_IDLE_STATE;

//Generic PM Header. Has message type and length of message.
typedef struct _pm_msg_header {
	PM_MESSAGE opcode;
	uint32_t len;
} pm_msg_header;

//TODO: For now every message will have
// a pointer to the message. More fieds to be added.
typedef struct _pm_msg_pc3Ready
{
	pm_msg_header header;
	void * buf;
} pm_msg_pc3Ready;

typedef struct _pm_msg_pc3Ready_body
{
	uint64_t wake_up_timer;
} pm_msg_pc3Ready_body;

typedef struct _pm_msg_host_abort_pcstate
{
	pm_msg_header header;
	void * buf;
} pm_msg_host_abort_pcstate;

typedef struct _pm_msg_uos_abortpc3
{
	pm_msg_header header;
	void * buf;
} pm_msg_uos_abortpc3;

typedef struct _pm_msg_pc6Request
{
	pm_msg_header header;
	void * buf;
} pm_msg_pc6Request;

typedef struct _pm_msg_abort_pc6Request
{
	pm_msg_header header;
	void * buf;
} pm_msg_abort_pc6Request;

typedef struct _pm_msg_pc6Ready
{
	pm_msg_header header;
	void * buf;
} pm_msg_pc6Ready;

typedef struct _pm_msg_pc6Query
{
	pm_msg_header header;
	void * buf;
} pm_msg_pc6Query;

typedef struct _pm_msg_pc6Response
{
	pm_msg_header header;
	void * buf;
} pm_msg_pc6Response;

typedef struct _pm_msg_wakeup_node
{
	pm_msg_header header;
	void * buf;
} pm_msg_wakeup_node;

typedef struct _pm_msg_node_ready
{
	pm_msg_header header;
	void * buf;
} pm_msg_node_ready;

typedef struct _pm_msg_configure_pcstates
{
	pm_msg_header header;
	void * buf;
} pm_msg_configure_pcstates;

typedef struct _pm_msg_get_latencies
{
	pm_msg_header header;
	void * buf;
} pm_msg_get_latencies;

typedef struct _pm_msg_latency_response
{
	pm_msg_header header;
	void * buf;
} pm_msg_latency_response;

typedef struct _pm_msg_policy_change
{
	pm_msg_header header;
	void * buf;
} pm_msg_policy_change;

typedef struct _pm_msg_power_off
{
	pm_msg_header header;
	void * buf;
} pm_msg_power_off;

typedef struct _pm_msg_unit_test
{
	pm_msg_header header;
	void * buf;
} pm_msg_unit_test;


#endif //__MIC_PM_COMMON__H

