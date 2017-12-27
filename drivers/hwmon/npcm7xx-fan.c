// SPDX-License-Identifier: GPL-2.0
// Copyright (c) 2014-2018 Nuvoton Technology corporation.

#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/device.h>
#include <linux/clk.h>
#include <linux/platform_device.h>
#include <linux/interrupt.h>
#include <linux/hwmon.h>
#include <linux/hwmon-sysfs.h>
#include <linux/sysfs.h>
#include <linux/of_irq.h>
#include <linux/of_address.h>

typedef struct {
	u8 u8ChannelNum;
	u8 u8FanPulsePerRev;
	u16 u16FanSpeedReading;
	u32 u32InputClock;
} sFanTachData;

#define NPCM750_MFT_CLKPS   255

/*
 * Get Fan Tach Timeout (base on clock 214843.75Hz, 1 cnt = 4.654us)
 * Timeout 94ms ~= 0x5000
 * (The minimum FAN speed could to support ~640RPM/pulse 1,
 * 320RPM/pulse 2, ...-- 10.6Hz)
 */
#define FAN_TACH_TIMEOUT   ((u16) 0x5000)

/*
 * Enable a background timer to poll fan tach value, (200ms * 4)
 * to polling all fan)
 */

/* 1 = 1 jiffies = 10 ms */
#define FAN_TACH_POLLING_INTERVAL 20

/* MFT General Defintion */
#define NPCM750_MFT_MAX_MODULE	8
#define NPCM750_CMPA	0
#define NPCM750_CMPB	1

#define NPCM750_MFT_MODE_5	4 /* Dual Independent Input Capture */

#define NPCM750_MFT_TCNT    ((u16) 0xFFFF)
#define NPCM750_MFT_TCPA    ((u16) (NPCM750_MFT_TCNT - FAN_TACH_TIMEOUT))
#define NPCM750_MFT_TCPB    ((u16) (NPCM750_MFT_TCNT - FAN_TACH_TIMEOUT))

#define NPCM750_MFT_NO_CLOCK_MODE		0
#define NPCM750_MFT_APB_CLOCK_MODE		1

#define DEFAULT_PULSE_PER_REVOLUTION	2

/* Fantach MFT registers */
#define MFT_REG_TCNT1(n)    ((void *) (MFT_REGS_BASE(n) + 0x00))
#define MFT_REG_TCRA(n)     ((void *) (MFT_REGS_BASE(n) + 0x02))
#define MFT_REG_TCRB(n)     ((void *) (MFT_REGS_BASE(n) + 0x04))
#define MFT_REG_TCNT2(n)    ((void *) (MFT_REGS_BASE(n) + 0x06))
#define MFT_REG_TPRSC(n)    ((void *) (MFT_REGS_BASE(n) + 0x08))
#define MFT_REG_TCKC(n)     ((void *) (MFT_REGS_BASE(n) + 0x0A))
#define MFT_REG_TMCTRL(n)   ((void *) (MFT_REGS_BASE(n) + 0x0C))
#define MFT_REG_TICTRL(n)   ((void *) (MFT_REGS_BASE(n) + 0x0E))
#define MFT_REG_TICLR(n)    ((void *) (MFT_REGS_BASE(n) + 0x10))
#define MFT_REG_TIEN(n)     ((void *) (MFT_REGS_BASE(n) + 0x12))
#define MFT_REG_TCPA(n)     ((void *) (MFT_REGS_BASE(n) + 0x14))
#define MFT_REG_TCPB(n)     ((void *) (MFT_REGS_BASE(n) + 0x16))
#define MFT_REG_TCPCFG(n)   ((void *) (MFT_REGS_BASE(n) + 0x18))
#define MFT_REG_TINASEL(n)  ((void *) (MFT_REGS_BASE(n) + 0x1A))
#define MFT_REG_TINBSEL(n)  ((void *) (MFT_REGS_BASE(n) + 0x1C))

#define NPCM750_TCKC_C2CSEL(mode)	(((mode) & GENMASK(2,0)) << 3)
#define NPCM750_TCKC_C1CSEL(mode)	((mode) & GENMASK(2,0))

#define NPCM750_TMCTRL_TBEN		BIT(6)
#define NPCM750_TMCTRL_TAEN		BIT(5)
#define NPCM750_TMCTRL_TBEDG	        BIT(4)
#define NPCM750_TMCTRL_TAEDG	        BIT(3)
#define NPCM750_TMCTRL_MDSEL(mode)	((mode) & GENMASK(2,0))

#define NPCM750_TICLR_CLEAR_ALL         GENMASK(5,0)
#define NPCM750_TICLR_TFCLR             BIT(5)
#define NPCM750_TICLR_TECLR             BIT(4)
#define NPCM750_TICLR_TDCLR             BIT(3)
#define NPCM750_TICLR_TCCLR             BIT(2)
#define NPCM750_TICLR_TBCLR             BIT(1)
#define NPCM750_TICLR_TACLR             BIT(0)

#define NPCM750_TIEN_ENABLE_ALL         GENMASK(5,0)
#define NPCM750_TIEN_TFIEN              BIT(5)
#define NPCM750_TIEN_TEIEN              BIT(4)
#define NPCM750_TIEN_TDIEN              BIT(3)
#define NPCM750_TIEN_TCIEN              BIT(2)
#define NPCM750_TIEN_TBIEN              BIT(1)
#define NPCM750_TIEN_TAIEN              BIT(0)

#define NPCM750_TICTRL_TFPND            BIT(5)
#define NPCM750_TICTRL_TEPND            BIT(4)
#define NPCM750_TICTRL_TDPND            BIT(3)
#define NPCM750_TICTRL_TCPND            BIT(2)
#define NPCM750_TICTRL_TBPND            BIT(1)
#define NPCM750_TICTRL_TAPND            BIT(0)

#define NPCM750_TCPCFG_HIBEN            BIT(7)
#define NPCM750_TCPCFG_EQBEN            BIT(6)
#define NPCM750_TCPCFG_LOBEN            BIT(5)
#define NPCM750_TCPCFG_CPBSEL           BIT(4)
#define NPCM750_TCPCFG_HIAEN            BIT(3)
#define NPCM750_TCPCFG_EQAEN            BIT(2)
#define NPCM750_TCPCFG_LOAEN            BIT(1)
#define NPCM750_TCPCFG_CPASEL           BIT(0)

#define NPCM750_TINASEL_FANIN_DEFAULT   (0x0)

#define FAN_TACH_DISABLE                            0xFF
#define FAN_TACH_INIT                               0x00
#define FAN_TACH_PREPARE_TO_GET_FIRST_CAPTURE       0x01
#define FAN_TACH_ENOUGH_SAMPLE                      0x02

/* maximum fan tach input support */
#define NPCM750_MAX_FAN_TACH	 16

/* Obtain the fan number */
#define NPCM750_FAN_TACH_INPUT(mft, cmp)	((mft << 1) + (cmp))

typedef struct {
	u8 u8FanStatusFlag;
	u8 u8FanPulsePerRev;
	u16 u16FanTachCnt;
	u32 u32FanTachCntTemp;
} sFanTachDev;

static int npcm750_fan_read(sFanTachData *pFanTachData);
static int mft_virt_addr;

#define MFT_REGS_BASE(n)    (mft_virt_addr + ((n) * 0x1000L))

/* for request irq use */
static u8 u8dummy;
int mft_irq[8];

/* Input clock */
static u32 u32InputClock;
static sFanTachDev S_npcm750_fantach[NPCM750_MAX_FAN_TACH];
static u8 S_npcm750_fantach_select;
static struct timer_list npcm750_fantach_timer;
static struct clk *mft_clk;

struct npcm750_fan_data {
	unsigned long clk_freq;
};

static inline void npcm750_fantach_start_capture(u8 mft, u8 cmp)
{
	u8 fan_id = 0;
	u8 reg_mode = 0;
	u8 reg_int = 0;

	fan_id = NPCM750_FAN_TACH_INPUT(mft, cmp);

	/* to check whether any fan tach is enable */
	if (S_npcm750_fantach[fan_id].u8FanStatusFlag != FAN_TACH_DISABLE) {
		/* reset status */
		S_npcm750_fantach[fan_id].u8FanStatusFlag = FAN_TACH_INIT;
		reg_int = ioread8((void *)MFT_REG_TIEN(mft));

		if (cmp == NPCM750_CMPA) {
			/* enable interrupt */
			iowrite8((u8) (reg_int | (NPCM750_TIEN_TAIEN |
						  NPCM750_TIEN_TEIEN)),
				 (void *)MFT_REG_TIEN(mft));

			reg_mode =
				NPCM750_TCKC_C1CSEL(NPCM750_MFT_APB_CLOCK_MODE)
				| ioread8((void *)MFT_REG_TCKC(mft));

			/* start to Capture */
			iowrite8(reg_mode, (void *)MFT_REG_TCKC(mft));
		} else {
			/* enable interrupt */
			iowrite8((u8) (reg_int | (NPCM750_TIEN_TBIEN |
						  NPCM750_TIEN_TFIEN)),
				 (void *)MFT_REG_TIEN(mft));

			reg_mode =
				NPCM750_TCKC_C2CSEL(NPCM750_MFT_APB_CLOCK_MODE)
				| ioread8((void *)MFT_REG_TCKC(mft));

			/* start to Capture */
			iowrite8(reg_mode, (void *)MFT_REG_TCKC(mft));
		}
	}
}

static void npcm750_fantach_polling(unsigned long data)
{
	int i;

	/* Polling two module per one round,
	 * MFT0 & MFT4 / MFT1 & MFT5 / MFT2 & MFT6 / MFT3 & MFT7
	 */
	//pr_info("npcm750_fantach_polling \n");
	for (i = S_npcm750_fantach_select; i < NPCM750_MFT_MAX_MODULE;
	      i = i+4) {
		/* clear the flag and reset the counter (TCNT) */
		iowrite8((u8) NPCM750_TICLR_CLEAR_ALL,
			 (void *) MFT_REG_TICLR(i));

		iowrite16(NPCM750_MFT_TCNT, (void *)MFT_REG_TCNT1(i));
		iowrite16(NPCM750_MFT_TCNT, (void *)MFT_REG_TCNT2(i));

		npcm750_fantach_start_capture(i, NPCM750_CMPA);
		npcm750_fantach_start_capture(i, NPCM750_CMPB);
	}

	S_npcm750_fantach_select++;
	S_npcm750_fantach_select &= 0x3;

	/* reset the timer interval */
	npcm750_fantach_timer.expires = jiffies + msecs_to_jiffies(20);
	add_timer(&npcm750_fantach_timer);
}

static int npcm750_fan_read(sFanTachData *pFanTachData)
{
	u8 fan_id = 0;

	fan_id = pFanTachData->u8ChannelNum;

	if (S_npcm750_fantach[fan_id].u16FanTachCnt != 0)
		pFanTachData->u16FanSpeedReading =
		S_npcm750_fantach[fan_id].u16FanTachCnt;
	else
		pFanTachData->u16FanSpeedReading = 0;

	return  0;
}

static inline void npcm750_fantach_compute(u8 mft, u8 cmp, u8 fan_id,
					   u8 flag_int, u8 flag_mode,
					   u8 flag_clear)
{
	u8  reg_int  = 0;
	u8  reg_mode = 0;
	u16 fan_cap  = 0;

	if (cmp == NPCM750_CMPA)
		fan_cap = ioread16((void *) MFT_REG_TCRA(mft));
	else
		fan_cap = ioread16((void *) MFT_REG_TCRB(mft));

	/* clear capature flag, H/W will auto reset the NPCM750_TCNTx */
	iowrite8((u8) flag_clear, (void *) MFT_REG_TICLR(mft));

	if (S_npcm750_fantach[fan_id].u8FanStatusFlag == FAN_TACH_INIT) {
		/* First capture, drop it */
		S_npcm750_fantach[fan_id].u8FanStatusFlag =
			FAN_TACH_PREPARE_TO_GET_FIRST_CAPTURE;

		/* reset counter */
		S_npcm750_fantach[fan_id].u32FanTachCntTemp = 0;
	} else if (S_npcm750_fantach[fan_id].u8FanStatusFlag <
		   FAN_TACH_ENOUGH_SAMPLE) {
		/*
		 * collect the enough sample,
		 * (ex: 2 pulse fan need to get 2 sample)
		 */
		S_npcm750_fantach[fan_id].u32FanTachCntTemp +=
			(NPCM750_MFT_TCNT - fan_cap);
		/*
		 * DEBUG_MSG("step 1, fan %d cnt %d total %x\n",
		 *	fan_id, (NPCM750_MFT_TCNT - fan_cap),
		 *	(u32) S_npcm750_fantach[fan_id].u32FanTachCntTemp);
		 */
		S_npcm750_fantach[fan_id].u8FanStatusFlag++;
	} else {
		/* get enough sample or fan disable */
		if (S_npcm750_fantach[fan_id].u8FanStatusFlag ==
		    FAN_TACH_ENOUGH_SAMPLE) {
			S_npcm750_fantach[fan_id].u32FanTachCntTemp +=
				(NPCM750_MFT_TCNT - fan_cap);
			/*
			 * DEBUG_MSG("step 2, fan %d cnt %d total %x\n",
			 *   fan_id, (NPCM750_MFT_TCNT - fan_cap),
			 *   (u32)S_npcm750_fantach[fan_id].u32FanTachCntTemp);
			 */

			/* compute finial average cnt per pulse */
			S_npcm750_fantach[fan_id].u16FanTachCnt
				= S_npcm750_fantach[fan_id].u32FanTachCntTemp /
				FAN_TACH_ENOUGH_SAMPLE;

			/*
			 * DEBUG_MSG("step 3 fan %d avg %d\n\n",
			 *   fan_id, S_npcm750_fantach[fan_id].u16FanTachCnt);
			 */
			S_npcm750_fantach[fan_id].u8FanStatusFlag =
				FAN_TACH_INIT;
		}

		reg_int =  ioread8((void *)MFT_REG_TIEN(mft));

		/* disable interrupt */
		iowrite8((u8) (reg_int & ~flag_int), (void *)MFT_REG_TIEN(mft));
		reg_mode =  ioread8((void *)MFT_REG_TCKC(mft));

		/* stop capturing */
		iowrite8((u8) (reg_mode & ~flag_mode),
			 (void *) MFT_REG_TCKC(mft));
	}
}

static inline void npcm750_check_cmp(u8 mft, u8 cmp, u8 flag)
{
	u8 reg_int = 0;
	u8 reg_mode = 0;
	u8 flag_timeout;
	u8 flag_cap;
	u8 flag_clear;
	u8 flag_int;
	u8 flag_mode;
	u8 fan_id;

	fan_id = NPCM750_FAN_TACH_INPUT(mft, cmp);

	if (cmp == NPCM750_CMPA) {
		flag_cap = NPCM750_TICTRL_TAPND;
		flag_timeout = NPCM750_TICTRL_TEPND;
		flag_int = (NPCM750_TIEN_TAIEN | NPCM750_TIEN_TEIEN);
		flag_mode = NPCM750_TCKC_C1CSEL(NPCM750_MFT_APB_CLOCK_MODE);
		flag_clear = NPCM750_TICLR_TACLR | NPCM750_TICLR_TECLR;
	} else {
		flag_cap = NPCM750_TICTRL_TBPND;
		flag_timeout = NPCM750_TICTRL_TFPND;
		flag_int = (NPCM750_TIEN_TBIEN | NPCM750_TIEN_TFIEN);
		flag_mode = NPCM750_TCKC_C2CSEL(NPCM750_MFT_APB_CLOCK_MODE);
		flag_clear = NPCM750_TICLR_TBCLR | NPCM750_TICLR_TFCLR;
	}

	if (flag & flag_timeout) {
		reg_int =  ioread8((void *)MFT_REG_TIEN(mft));

		/** disable interrupt */
		iowrite8((u8) (reg_int & ~flag_int), (void *)MFT_REG_TIEN(mft));

		/** clear interrup flag */
		iowrite8((u8) flag_clear, (void *) MFT_REG_TICLR(mft));

		reg_mode =  ioread8((void *)MFT_REG_TCKC(mft));

		/** stop capturing */
		iowrite8((u8) (reg_mode & ~flag_mode),
			 (void *) MFT_REG_TCKC(mft));

		/*
		 *  If timeout occurs (FAN_TACH_TIMEOUT), the fan doesn't
		 *  connect or speed is lower than 10.6Hz (320RPM/pulse2).
		 *  In these situation, the RPM output should be zero.
		 */
		S_npcm750_fantach[fan_id].u16FanTachCnt = 0;
		//DEBUG_MSG("%s : it is timeout fan_id %d\n", __func__, fan_id);
	} else {
	    /** input capture is occurred */
		if (flag & flag_cap)
			npcm750_fantach_compute(mft, cmp, fan_id, flag_int,
						flag_mode, flag_clear);
	}
}

static irqreturn_t npcm750_mft0_isr(int irq, void *dev_id)
{
	u8 flag = 0;
	int module;

	module = irq - mft_irq[0];
	flag = ioread8((void *)(void *) MFT_REG_TICTRL(module));
	if (flag > 0) {
		npcm750_check_cmp(module, NPCM750_CMPA, flag);
		npcm750_check_cmp(module, NPCM750_CMPB, flag);
		return IRQ_HANDLED;
	}

	return IRQ_NONE;
}

static int npcm7xx_read_fan(struct device *dev, u32 attr, int channel,
			     long *val)
{
	sFanTachData FanTachData;

	FanTachData.u8ChannelNum = (u8)channel;

	switch (attr) {
	case hwmon_fan_input:
		npcm750_fan_read(&FanTachData);
		if (FanTachData.u16FanSpeedReading <= 0)
		{
			*val = 0;
			return FanTachData.u16FanSpeedReading;
		}

		/*Convert the raw reading to RPM*/
		if ((FanTachData.u16FanSpeedReading > 0) && 
		    S_npcm750_fantach[channel].u8FanPulsePerRev > 0)                    
			*val = (long)((u32InputClock * 60)/
				    (FanTachData.u16FanSpeedReading * 
				     S_npcm750_fantach[channel].u8FanPulsePerRev));
		return 0;
	default:
		return -EOPNOTSUPP;
	}
}

static int npcm7xx_write_fan(struct device *dev, u32 attr, int channel,
			      long val)
{
	//struct npcm7xx_pwm_fan_data *data = dev_get_drvdata(dev);
	int err = 0;

	switch (attr) {
	case hwmon_fan_target:
		//err = npcm7xx_pwm_config_set(data, channel, (u16)val);
		break;
	default:
		err = -EOPNOTSUPP;
		break;
	}

	return err;
}

static umode_t npcm7xx_fan_is_visible(const void *_data, u32 attr, int channel)
{
	switch (attr) {
	case hwmon_fan_input:
		return 0644;
	default:
		return 0;
	}
}

static int npcm7xx_read(struct device *dev, enum hwmon_sensor_types type,
			 u32 attr, int channel, long *val)
{
	switch (type) {
	case hwmon_fan:
		return npcm7xx_read_fan(dev, attr, channel, val);
	default:
		return -EOPNOTSUPP;
	}
}

static int npcm7xx_write(struct device *dev, enum hwmon_sensor_types type,
			  u32 attr, int channel, long val)
{
	switch (type) {
	case hwmon_fan:
		return npcm7xx_write_fan(dev, attr, channel, val);
	default:
		return -EOPNOTSUPP;
	}
}

static umode_t npcm7xx_is_visible(const void *data,
				   enum hwmon_sensor_types type,
				   u32 attr, int channel)
{
	switch (type) {
	case hwmon_fan:
		return npcm7xx_fan_is_visible(data, attr, channel);
	default:
		return 0;
	}
}

static const u32 npcm7xx_fan_config[] = {
	HWMON_F_INPUT | HWMON_F_TARGET,
	HWMON_F_INPUT | HWMON_F_TARGET,
	HWMON_F_INPUT | HWMON_F_TARGET,
	HWMON_F_INPUT | HWMON_F_TARGET,
	HWMON_F_INPUT | HWMON_F_TARGET,
	HWMON_F_INPUT | HWMON_F_TARGET,
	HWMON_F_INPUT | HWMON_F_TARGET,
	HWMON_F_INPUT | HWMON_F_TARGET,
	HWMON_F_INPUT,
	HWMON_F_INPUT,
	HWMON_F_INPUT,
	HWMON_F_INPUT,
	HWMON_F_INPUT,
	HWMON_F_INPUT,
	HWMON_F_INPUT,
	HWMON_F_INPUT,
	0
};

static const struct hwmon_channel_info npcm7xx_fan = {
	.type = hwmon_fan,
	.config = npcm7xx_fan_config,
};

static const struct hwmon_channel_info *npcm7xx_info[] = {
	&npcm7xx_fan,
	NULL
};

static const struct hwmon_ops npcm7xx_hwmon_ops = {
	.is_visible = npcm7xx_is_visible,
	.read = npcm7xx_read,
	.write = npcm7xx_write,
};

static const struct hwmon_chip_info npcm7xx_chip_info = {
	.ops = &npcm7xx_hwmon_ops,
	.info = npcm7xx_info,
};

static int npcm750_fan_probe(struct platform_device *pdev)
{
	u32 apb_clk_src;
	int ret = 0;
	struct device *dev = &pdev->dev;
	struct device_node *np;
	struct npcm750_fan_data *priv;
	struct resource res;
	struct device *hwmon;
	int i;

	priv = devm_kzalloc(dev, sizeof(*priv), GFP_KERNEL);
	if (!priv)
		return -ENOMEM;

	np = dev->of_node;

	ret = of_address_to_resource(np, 0, &res);
	if (ret) {
		pr_err("\t\t\t of_address_to_resource fail ret %d\n", ret);
		return -EINVAL;
	}

	mft_virt_addr = (int)ioremap(res.start, resource_size(&res));

	if (!mft_virt_addr) {
		pr_err("\t\t\t mft_virt_addr fail\n");
		return -ENOMEM;
	}

	/*DEBUG_MSG("MFT base is 0x%08X ,res.start 0x%08X\n",
		  (u32)mft_virt_addr, res.start);*/

	mft_clk = devm_clk_get(&pdev->dev, NULL);

	if (IS_ERR(mft_clk)) {
		pr_err(" MFT (FAN) probe failed: can't read clk.\n");
		return -ENODEV;
	}

	clk_prepare_enable(mft_clk);

	for (i = 0; i < NPCM750_MFT_MAX_MODULE; i++) {
		/* stop MFT0~7 clock */
		iowrite8((u8) NPCM750_MFT_NO_CLOCK_MODE,
			 (void *)MFT_REG_TCKC(i));

		/* disable all interrupt */
		iowrite8((u8) 0x00, (void *)MFT_REG_TIEN(i));

		/* clear all interrupt */
		iowrite8((u8) NPCM750_TICLR_CLEAR_ALL,
			 (void *)MFT_REG_TICLR(i));

		/* set MFT0~7 clock prescaler */
		iowrite8((u8) NPCM750_MFT_CLKPS, (void *)MFT_REG_TPRSC(i));

		/* set MFT0~7 mode (high-to-low transition) */
		iowrite8(
			(u8) (
			      NPCM750_TMCTRL_MDSEL(NPCM750_MFT_MODE_5) |
			      NPCM750_TMCTRL_TBEN |
			      NPCM750_TMCTRL_TAEN
			      ),
			(void *) MFT_REG_TMCTRL(i)
			);

		/* set MFT0~7 Initial Count/Cap */
		iowrite16(NPCM750_MFT_TCNT, (void *)MFT_REG_TCNT1(i));
		iowrite16(NPCM750_MFT_TCNT, (void *)MFT_REG_TCNT2(i));

		/* set MFT0~7 compare (equal to count) */
		iowrite8((u8)(NPCM750_TCPCFG_EQAEN | NPCM750_TCPCFG_EQBEN),
			  (void *)MFT_REG_TCPCFG(i));

		/* set MFT0~7 compare value */
		iowrite16(NPCM750_MFT_TCPA, (void *)MFT_REG_TCPA(i));
		iowrite16(NPCM750_MFT_TCPB, (void *)MFT_REG_TCPB(i));

		/* set MFT0~7 fan input FANIN 0~15 */
		iowrite8((u8) NPCM750_TINASEL_FANIN_DEFAULT,
			 (void *)MFT_REG_TINASEL(i));
		iowrite8((u8) NPCM750_TINASEL_FANIN_DEFAULT,
			 (void *)MFT_REG_TINBSEL(i));
	}

	/** fan tach structure initialization */
	S_npcm750_fantach_select = 0;
	for (i = 0; i < NPCM750_MAX_FAN_TACH; i++) {
		S_npcm750_fantach[i].u8FanStatusFlag = FAN_TACH_DISABLE;
		S_npcm750_fantach[i].u8FanPulsePerRev =
			DEFAULT_PULSE_PER_REVOLUTION;
		S_npcm750_fantach[i].u16FanTachCnt = 0;
	}

	for (i = 0; i < 8; i++) {
		mft_irq[i] = platform_get_irq(pdev, i);
		if (!mft_irq[i]) {
			pr_err("%s - failed to map irq %d\n", __func__, i);
			return (-EAGAIN);
		}
	}

	if (request_irq(mft_irq[0], (irq_handler_t) npcm750_mft0_isr, 0,
			"NPCM750-MFT0", (void *) &u8dummy)) {
		pr_err("NPCM750: register irq MFT0 failed\n");
		return (-EAGAIN);
	}

	if (request_irq(mft_irq[1], (irq_handler_t) npcm750_mft0_isr, 0,
		    "NPCM750-MFT1", (void *) &u8dummy)) {
		pr_err("NPCM750: register irq MFT1 failed\n");
		free_irq(mft_irq[0], (void *) &u8dummy);
		return (-EAGAIN);
	}

	if (request_irq(mft_irq[2], (irq_handler_t) npcm750_mft0_isr, 0,
		    "NPCM750-MFT2", (void *) &u8dummy)) {
		pr_err("NPCM750: register irq MFT2 failed\n");
		free_irq(mft_irq[0], (void *) &u8dummy);
		free_irq(mft_irq[1], (void *) &u8dummy);
		return (-EAGAIN);
	}

	if (request_irq(mft_irq[3], (irq_handler_t) npcm750_mft0_isr, 0,
		    "NPCM750-MFT3", (void *) &u8dummy)) {
		pr_err("NPCM750: register irq MFT3 failed\n");
		free_irq(mft_irq[0], (void *) &u8dummy);
		free_irq(mft_irq[1], (void *) &u8dummy);
		free_irq(mft_irq[2], (void *) &u8dummy);
		return (-EAGAIN);
	}

	if (request_irq(mft_irq[4], (irq_handler_t) npcm750_mft0_isr, 0,
		    "NPCM750-MFT4", (void *) &u8dummy)) {
		pr_err("NPCM750: register irq MFT4 failed\n");
		free_irq(mft_irq[0], (void *) &u8dummy);
		free_irq(mft_irq[1], (void *) &u8dummy);
		free_irq(mft_irq[2], (void *) &u8dummy);
		free_irq(mft_irq[3], (void *) &u8dummy);
		return (-EAGAIN);
	}

	if (request_irq(mft_irq[5], (irq_handler_t) npcm750_mft0_isr, 0,
		    "NPCM750-MFT5", (void *) &u8dummy)) {
		pr_err("NPCM750: register irq MFT5 failed\n");
		free_irq(mft_irq[0], (void *) &u8dummy);
		free_irq(mft_irq[1], (void *) &u8dummy);
		free_irq(mft_irq[2], (void *) &u8dummy);
		free_irq(mft_irq[3], (void *) &u8dummy);
		free_irq(mft_irq[4], (void *) &u8dummy);
		return (-EAGAIN);
	}

	if (request_irq(mft_irq[6], (irq_handler_t) npcm750_mft0_isr, 0,
		    "NPCM750-MFT6", (void *) &u8dummy)) {
		pr_err("NPCM750: register irq MFT6 failed\n");
		free_irq(mft_irq[0], (void *) &u8dummy);
		free_irq(mft_irq[1], (void *) &u8dummy);
		free_irq(mft_irq[2], (void *) &u8dummy);
		free_irq(mft_irq[3], (void *) &u8dummy);
		free_irq(mft_irq[4], (void *) &u8dummy);
		free_irq(mft_irq[5], (void *) &u8dummy);
		return (-EAGAIN);
	}

	if (request_irq(mft_irq[7], (irq_handler_t) npcm750_mft0_isr, 0,
		    "NPCM750-MFT7", (void *) &u8dummy)) {
		pr_err("NPCM750: register irq MFT7 failed\n");
		free_irq(mft_irq[0], (void *) &u8dummy);
		free_irq(mft_irq[1], (void *) &u8dummy);
		free_irq(mft_irq[2], (void *) &u8dummy);
		free_irq(mft_irq[3], (void *) &u8dummy);
		free_irq(mft_irq[4], (void *) &u8dummy);
		free_irq(mft_irq[5], (void *) &u8dummy);
		free_irq(mft_irq[6], (void *) &u8dummy);
		return (-EAGAIN);
	}

	/** initialize fan tach polling timer */
	npcm750_fantach_timer.data = 0;
	npcm750_fantach_timer.function = &npcm750_fantach_polling;

	/** set timer interval */
	npcm750_fantach_timer.expires = jiffies +  msecs_to_jiffies(20);

	init_timer(&npcm750_fantach_timer);
	add_timer(&npcm750_fantach_timer);

	apb_clk_src = clk_get_rate(mft_clk);

	pr_info("[FAN] APB4: %d\n", (int)apb_clk_src);

	/* Fan tach input clock = APB clock / prescalar, default is 255. */
	u32InputClock = apb_clk_src / (NPCM750_MFT_CLKPS + 1);

	pr_info("[FAN] PWM: %d\n", (int) u32InputClock);
	pr_info("[FAN] InputClock: %d\n", (int) u32InputClock);

	hwmon = devm_hwmon_device_register_with_info(dev, "npcm7xx_fan", priv,
					      &npcm7xx_chip_info, NULL);

	if (IS_ERR(hwmon)) {
		pr_err("FAN Driver failed - "
		       "devm_hwmon_device_register_with_groups failed\n");
		return PTR_ERR(hwmon);
	}

	pr_info("NPCM750 FAN Driver probed\n");

	for (i = 0; i < NPCM750_MAX_FAN_TACH; i++)
		S_npcm750_fantach[i].u8FanStatusFlag = FAN_TACH_INIT;

	return 0;
}

/* work with hotplug and coldplug */
MODULE_ALIAS("platform:npcm750-fan");

static const struct of_device_id of_fan_match_table[] = {
	{ .compatible = "nuvoton,npcm750-fan", },
	{},
};
MODULE_DEVICE_TABLE(of, of_fan_match_table);

static struct platform_driver npcm750_fan_driver = {
	.probe		= npcm750_fan_probe,
	.driver		= {
		.name	= "npcm750_fan",
		.of_match_table = of_fan_match_table,
	},
};

module_platform_driver(npcm750_fan_driver);

MODULE_DESCRIPTION("Nuvoton NPCM750 FAN Driver");
MODULE_AUTHOR("Tomer Maimon <tomer.maimon@nuvoton.com>");
MODULE_LICENSE("GPL v2");
