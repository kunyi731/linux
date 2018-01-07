/*
 * Copyright (c) 2014-2017 Nuvoton Technology corporation.
 *
 * Released under the GPLv2 only.
 * SPDX-License-Identifier: GPL-2.0
 */

#include <linux/io.h>
#include <linux/uaccess.h>
#include <linux/module.h>
#include <linux/device.h>
#include <linux/fs.h>
#include <linux/signal.h>
#include <linux/spinlock.h>
#include <linux/kernel.h>
#include <linux/platform_device.h>
#include <linux/delay.h>

#include <linux/iio/iio.h>
#include <linux/iio/driver.h>
#include <linux/iio/sysfs.h>

#include <linux/clk.h>

#include <linux/regmap.h>
#include <linux/mfd/syscon.h>

static struct regmap *rst_regmap;

#define  IPSRST1_OFFSET 0x020

struct npcm7xx_adc {
	struct device *dev;
	void __iomem *regs;
	struct clk *adc_clk;
	u32 vref;
	u32 adc_clk_rate;
	u32 ADCReading;
	u8  ADCChannelNum;
};

/* ADC registers */
#define NPCM7XX_ADCCON	 0x00
#define NPCM7XX_ADCDATA	 0x04

/* ADCCON Register Bits */
#define NPCM7XX_ADCCON_ADC_INT_EN	BIT(21)
#define NPCM7XX_ADCCON_REFSEL		BIT(19)
#define NPCM7XX_ADCCON_ADC_INT		BIT(18)
#define NPCM7XX_ADCCON_ADC_EN		BIT(17)
#define NPCM7XX_ADCCON_ADC_RST		BIT(16)
#define NPCM7XX_ADCCON_ADC_CONV		BIT(13)

#define NPCM7XX_ADCCON_ADCMUX(x)		(((x) & 0x0F)<<24)
#define NPCM7XX_ADCCON_ADC_DIV(x)		(((x) & 0xFF)<<24)
#define NPCM7XX_ADCCON_ADC_DATA_MASK(x)		((x) & 0x3FF)
#define NPCM7XX_ADCCON_MUXMASK			(0x0F<<24)

/* ADC General Defintion */
#define NPCM7XX_ADC_INPUT_CLK_DIV		0
#define NPCM7XX_ADC_CONVERT_MAX_RETRY_CNT	1000

#define NPCM7XX_ADC_MAX_CHNL_NUM	8

#define NPCM7XX_ADC_CHNL0_ADCI0		0
#define NPCM7XX_ADC_CHNL1_ADCI1		1
#define NPCM7XX_ADC_CHNL2_ADCI2		2
#define NPCM7XX_ADC_CHNL3_ADCI3		3
#define NPCM7XX_ADC_CHNL4_ADCI4		4
#define NPCM7XX_ADC_CHNL5_ADCI5		5
#define NPCM7XX_ADC_CHNL6_ADCI6		6
#define NPCM7XX_ADC_CHNL7_ADCI7		7

#define ADC_MAX_CLOCK 12500000
#define VREF_MVOLT 2048		//vref = 2.000v

#define NPCM7XX_ADC_CHAN(_idx) {			\
	.type = IIO_VOLTAGE,				\
	.indexed = 1,					\
	.channel = (_idx),				\
	.info_mask_separate = BIT(IIO_CHAN_INFO_RAW),	\
}

static const struct iio_chan_spec npcm7xx_adc_iio_channels[] = {
	NPCM7XX_ADC_CHAN(0),
	NPCM7XX_ADC_CHAN(1),
	NPCM7XX_ADC_CHAN(2),
	NPCM7XX_ADC_CHAN(3),
	NPCM7XX_ADC_CHAN(4),
	NPCM7XX_ADC_CHAN(5),
	NPCM7XX_ADC_CHAN(6),
	NPCM7XX_ADC_CHAN(7),
};

//#define ADC_DEBUG

#ifdef ADC_DEBUG
static char *S_ADCChnlString[] = {
	"ADCI0", "ADCI1", "ADCI2", "ADCI3", "ADCI4", "ADCI5", "ADCI6", "ADCI7"
};
#define PDEBUG(fmt, args...) pr_info("aess_adcdrv %s() " fmt, __func__, ##args)
#else
#define PDEBUG(fmt, args...)
#endif
#define PERROR(fmt, args...) pr_err("aess_adcdrv %s(): " fmt, __func__, ##args)

static int adcsensor_read(struct npcm7xx_adc *info)
{
	u8  u8ChannelNum = info->ADCChannelNum;
	u32 regtemp = 0;
	int cnt = 0;

	/* Select ADC channal */
	regtemp = ioread32(info->regs + NPCM7XX_ADCCON);
	regtemp &= ~NPCM7XX_ADCCON_MUXMASK;

	iowrite32((u32) (regtemp | NPCM7XX_ADCCON_ADCMUX(u8ChannelNum) |
			 NPCM7XX_ADCCON_ADC_EN | NPCM7XX_ADCCON_REFSEL),
		  info->regs + NPCM7XX_ADCCON);

	/* Activate convert the ADC input */
	regtemp = ioread32(info->regs + NPCM7XX_ADCCON);
	iowrite32((u32) (regtemp | NPCM7XX_ADCCON_ADC_CONV),
		  info->regs + NPCM7XX_ADCCON);

	/* Wait value */
	while (((regtemp = ioread32(info->regs + NPCM7XX_ADCCON)) &
	       NPCM7XX_ADCCON_ADC_CONV) != 0) {
		if (cnt < NPCM7XX_ADC_CONVERT_MAX_RETRY_CNT)
			cnt++;
		else {
			PERROR("ADC CONVERT FAIL - Timeout\n");
			PERROR("NPCM7XX_ADCCON=0x%08X, ADC_MUX=%d u8ChannelNum="
			       "%d!!\n", regtemp, (regtemp>>24)&0xF,
			       u8ChannelNum);
			if (((regtemp>>24) & 0xF) != u8ChannelNum)
				PERROR("ADC_MUX != u8ChannelNum, I suspect that"
				       " 2 threads are trying to access this "
				       "read and it is not protected "
				       "by mutex\n");

			/* if convertion failed - reset ADC module */
			regmap_write(rst_regmap, IPSRST1_OFFSET, 0x08000000);
			msleep(100);
			regmap_write(rst_regmap, IPSRST1_OFFSET, 0x0);
			msleep(100);
			PERROR("RESET ADC Complete\n");
			return (-EAGAIN);
		}
	}

/* When an ADC conversion operation finished, a delay must be added before
 * the next conversion operation.
 * The delay depend on the ADC clock:
 * When ADC clock is 0.5 MHz: delay is 4 us.
 * When ADC clock is 12.5 MHz: delay is 160 ns.
 *
 * In the current driver the ADC clock is 12.5MHz, so delay is not needed.
 * (already the R/W register take more than 160ns)
 * If the ADC clock will be lower than 12.5MHz please add delay according
 * the details above
 * udelay(conv_delay);
 */

	/* finish to convert */
	info->ADCReading = NPCM7XX_ADCCON_ADC_DATA_MASK
		(ioread32(info->regs + NPCM7XX_ADCDATA));

	PDEBUG("[%d_%s] ADCReading=%ld [%ldmV]\n",
	       u8ChannelNum, S_ADCChnlString[u8ChannelNum],
	       (long int)info->ADCReading,
	       (long int)(info->ADCReading * info->vref / 1024));

	return 0;
}

static int npcm7xx_adc_read_raw(struct iio_dev *indio_dev,
			struct iio_chan_spec const *chan, int *val, int *val2,
			long mask)
{
	int err_check;
	struct npcm7xx_adc *info = iio_priv(indio_dev);

	info->ADCChannelNum = chan->channel;

	switch (mask) {
	case IIO_CHAN_INFO_RAW:

		switch (info->ADCChannelNum) {
		case NPCM7XX_ADC_CHNL0_ADCI0:
		case NPCM7XX_ADC_CHNL1_ADCI1:
		case NPCM7XX_ADC_CHNL2_ADCI2:
		case NPCM7XX_ADC_CHNL3_ADCI3:
		case NPCM7XX_ADC_CHNL4_ADCI4:
		case NPCM7XX_ADC_CHNL5_ADCI5:
		case NPCM7XX_ADC_CHNL6_ADCI6:
		case NPCM7XX_ADC_CHNL7_ADCI7:
			mutex_lock(&indio_dev->mlock);
			err_check = adcsensor_read(info);
			PDEBUG("%d = aess_adcsensor_read()\n", err_check);
			if (err_check) {
				PERROR("err_check %d\n", err_check);
				mutex_unlock(&indio_dev->mlock);
				return err_check;
			}
			*val = info->ADCReading;
			mutex_unlock(&indio_dev->mlock);
			return IIO_VAL_INT;
		default:
			PERROR("aess_adcsensor_ioctl, Unsupport channel number"
			       " [%d]!\n", info->ADCChannelNum);
			err_check = -ENODEV;
			break;
		}
		break;

	default:
		PERROR("aess_adcsensor_ioctl, command error!!!\n");
		err_check = -EINVAL;
	}

	/* 0->ok, minus->fail */
	return err_check;
}

static const struct iio_info npcm7xx_adc_iio_info = {
	.driver_module = THIS_MODULE,
	.read_raw = &npcm7xx_adc_read_raw,
};

static const struct of_device_id npcm7xx_adc_match[] = {
	{ .compatible = "nuvoton,npcm750-adc", },
	{ /* sentinel */ }
};
MODULE_DEVICE_TABLE(of, npcm7xx_adc_match);


static int npcm7xx_adc_probe(struct platform_device *pdev)
{
	struct npcm7xx_adc *info;
	struct iio_dev *indio_dev;
	struct resource *mem;
	struct device *dev = &pdev->dev;
	int ret;
	u32 regtemp = 0;

	indio_dev = devm_iio_device_alloc(&pdev->dev, sizeof(*info));
	if (!indio_dev) {
		dev_err(&pdev->dev, "Failed allocating iio device\n");
		return -ENOMEM;
	}

	info = iio_priv(indio_dev);
	info->dev = &pdev->dev;

	mem = platform_get_resource(pdev, IORESOURCE_MEM, 0);
	info->regs = devm_ioremap_resource(&pdev->dev, mem);
	if (IS_ERR(info->regs)) {
		ret = PTR_ERR(info->regs);
		dev_err(&pdev->dev, "Failed to remap adc memory, err = %d\n",
			ret);
		return ret;
	}

	ret = of_property_read_u32(dev->of_node, "vref", &info->vref);
	if (ret) {
		dev_err(&pdev->dev, "Failed getting reference voltage, Assuming"
				   " reference voltage 2V(2048)\n");
		info->vref = VREF_MVOLT;
		ret = 0;
	}

	info->adc_clk = devm_clk_get(&pdev->dev, "clk_adc");
	if (IS_ERR(info->adc_clk)) {
		dev_err(&pdev->dev, "ADC clock failed: can't read clk. "
				    "Assuming ADC clock Rate 12.5MHz\n");
		info->adc_clk_rate = ADC_MAX_CLOCK;
	} else {
		/* calculate ADC clock divider */
		regtemp = ioread32(info->regs + NPCM7XX_ADCCON);
		regtemp = regtemp >> 1;
		regtemp &= 0xff;

		info->adc_clk_rate = clk_get_rate(info->adc_clk) /
			((regtemp+1)*2);
	}

	rst_regmap = syscon_regmap_lookup_by_compatible("nuvoton,npcm750-rst");
	if (IS_ERR(rst_regmap)) {
		pr_err("%s: failed to find nuvoton,npcm750-rst\n", __func__);
		return IS_ERR(rst_regmap);
	}

	pr_info("ADC clock Rate %d\n", info->adc_clk_rate);

	/** Enable the ADC Module **/
	iowrite32((u32) NPCM7XX_ADCCON_ADC_EN, info->regs + NPCM7XX_ADCCON);

	platform_set_drvdata(pdev, indio_dev);

	indio_dev->name = dev_name(&pdev->dev);
	indio_dev->dev.parent = &pdev->dev;
	indio_dev->info = &npcm7xx_adc_iio_info;
	indio_dev->modes = INDIO_DIRECT_MODE;
	indio_dev->channels = npcm7xx_adc_iio_channels;
	indio_dev->num_channels = ARRAY_SIZE(npcm7xx_adc_iio_channels);

	ret = iio_device_register(indio_dev);
	if (ret) {
		dev_err(&pdev->dev, "Couldn't register the device.\n");
		clk_disable_unprepare(info->adc_clk);
		return ret;
	}

	pr_info("NPCM7XX ADC driver probed\n");

	return 0;
}

static int npcm7xx_adc_remove(struct platform_device *pdev)
{
	struct iio_dev *indio_dev = platform_get_drvdata(pdev);
	struct npcm7xx_adc *info = iio_priv(indio_dev);
	u32 regtemp = 0;

	regtemp = ioread32(info->regs + NPCM7XX_ADCCON);

	/* Disable the ADC Module */
	iowrite32((u32) (regtemp & ~NPCM7XX_ADCCON_ADC_EN),
		  info->regs + NPCM7XX_ADCCON);

	iio_device_unregister(indio_dev);

	pr_info("NPCM7XX ADC driver removed\n");

	return 0;
}

static struct platform_driver npcm7xx_adc_driver = {
	.probe		= npcm7xx_adc_probe,
	.remove		= npcm7xx_adc_remove,
	.driver		= {
		.name	= "npcm7xx_adc",
		.of_match_table = npcm7xx_adc_match,
	},
};

module_platform_driver(npcm7xx_adc_driver);

MODULE_DESCRIPTION("NPCM7XX ADC Sensor Driver");
MODULE_AUTHOR("Tomer Maimon <tomer.maimon@nuvoton.com>");
MODULE_LICENSE("GPL v2");
