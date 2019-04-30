#include <stdint.h>
#include <stdio.h>

typedef signed int s32;
typedef unsigned int u32;
typedef signed long s64;
typedef unsigned long u64;
typedef u64 vaddr_t;
#define PRIvaddr PRIx64
typedef u64 paddr_t;
#define INVALID_PADDR (~0UL)
#define PRIpaddr "016lx"
typedef u64 register_t;
#define PRIregister "lx"
#define LONG_BYTEORDER 3
#define BYTES_PER_LONG (1 << LONG_BYTEORDER)
#define BITS_PER_LONG (BYTES_PER_LONG << 3)
#define PAGE_SHIFT              12
#define PAGE_SIZE ((unsigned long)1 << PAGE_SHIFT)
#define MB(_mb)     ((unsigned long long)(_mb) << 20)
#define BITOP_WORD(nr)		((nr) / BITS_PER_LONG)
#define ffz(x)  __ffs(~(x))

#define MAX_ORDER 18 /* 2^20 contiguous pages */
unsigned long max_pdx;
unsigned long pfn_pdx_bottom_mask = ~0UL;
unsigned long ma_va_bottom_mask = ~0UL;
unsigned long pfn_top_mask = 0;
unsigned long ma_top_mask = 0;
unsigned long pfn_hole_mask = 0;
unsigned int pfn_pdx_hole_shift = 0;
unsigned long tot_size = 0;

struct page_list_entry
{
    unsigned long next, prev;
};

struct page_info
{
    /* Each frame can be threaded onto a doubly-linked list. */
    struct page_list_entry list;

    /* Reference count and various PGC_xxx flags and fields. */
    unsigned long count_info;

    /* Context-dependent fields follow... */
    union {
        /* Page is in use: ((count_info & PGC_count_mask) != 0). */
        struct {
            /* Type reference count and various PGT_xxx flags and fields. */
            unsigned long type_info;
        } inuse;
        /* Page is on a free list: ((count_info & PGC_count_mask) == 0). */
        union {
            struct {
                /*
                 * Index of the first *possibly* unscrubbed page in the buddy.
                 * One more bit than maximum possible order to accommodate
                 * INVALID_DIRTY_IDX.
                 */
#define INVALID_DIRTY_IDX ((1UL << (MAX_ORDER + 1)) - 1)
                unsigned long first_dirty:MAX_ORDER + 1;

                /* Do TLBs need flushing for safety before next page use? */
                char need_tlbflush:1;

#define BUDDY_NOT_SCRUBBING    0
#define BUDDY_SCRUBBING        1
#define BUDDY_SCRUB_ABORT      2
                unsigned long scrub_state:2;
            };

            unsigned long val;
            } free;

    } u;

    union {
        /* Page is in use, but not as a shadow. */
        struct {
            /* Owner of this page (zero if page is anonymous). */
            unsigned long ppp;
        } inuse;

        /* Page is on a free list. */
        struct {
            /* Order-size of the free chunk this page is the head of. */
            unsigned int order;
        } free;

    } v;

    union {
        /*
         * Timestamp from 'TLB clock', used to avoid extra safety flushes.
         * Only valid for: a) free pages, and b) pages with zero type count
         */
        u32 tlbflush_timestamp;
    };
    u64 pad;
};

struct membank {
    paddr_t start;
    paddr_t size;
};

struct meminfo {
    int nr_banks;
    struct membank bank[32];
};

struct bootinfo {
    struct meminfo mem;
};

struct bootinfo bootinfo;

unsigned long find_next_zero_bit(const unsigned long *addr, unsigned long size,
				 unsigned long offset)
{
	const unsigned long *p = addr + BITOP_WORD(offset);
	unsigned long result = offset & ~(BITS_PER_LONG-1);
	unsigned long tmp;

	if (offset >= size)
		return size;
	size -= result;
	offset %= BITS_PER_LONG;
	if (offset) {
		tmp = *(p++);
		tmp |= ~0UL >> (BITS_PER_LONG - offset);
		if (size < BITS_PER_LONG)
			goto found_first;
		if (~tmp)
			goto found_middle;
		size -= BITS_PER_LONG;
		result += BITS_PER_LONG;
	}
	while (size & ~(BITS_PER_LONG-1)) {
		if (~(tmp = *(p++)))
			goto found_middle;
		result += BITS_PER_LONG;
		size -= BITS_PER_LONG;
	}
	if (!size)
		return result;
	tmp = *p;

found_first:
	tmp |= ~0UL << size;
	if (tmp == ~0UL)	/* Are any bits zero? */
		return result + size;	/* Nope. */
found_middle:
	return result + ffz(tmp) - 1;
}

unsigned long find_next_bit(const unsigned long *addr, unsigned long size,
			    unsigned long offset)
{
	const unsigned long *p = addr + BITOP_WORD(offset);
	unsigned long result = offset & ~(BITS_PER_LONG-1);
	unsigned long tmp;

	if (offset >= size)
		return size;
	size -= result;
	offset %= BITS_PER_LONG;
	if (offset) {
		tmp = *(p++);
		tmp &= (~0UL << offset);
		if (size < BITS_PER_LONG)
			goto found_first;
		if (tmp)
			goto found_middle;
		size -= BITS_PER_LONG;
		result += BITS_PER_LONG;
	}
	while (size & ~(BITS_PER_LONG-1)) {
		if ((tmp = *(p++)))
			goto found_middle;
		result += BITS_PER_LONG;
		size -= BITS_PER_LONG;
	}
	if (!size)
		return result;
	tmp = *p;

found_first:
	tmp &= (~0UL >> (BITS_PER_LONG - size));
	if (tmp == 0UL)		/* Are any bits set? */
		return result + size;	/* Nope. */
found_middle:
	return result + __ffs(tmp) - 1;
}

static u64 fill_mask(u64 mask)
{
    while (mask & (mask + 1))
        mask |= mask + 1;

    return mask;
}

u64 pdx_init_mask(u64 base_addr)
{
    return fill_mask(base_addr - 1);
}

u64 pdx_region_mask(u64 base, u64 len)
{
    return fill_mask(base ^ (base + len - 1));
}

static unsigned long pfn_to_pdx(unsigned long pfn)
{
    return (pfn & pfn_pdx_bottom_mask) |
           ((pfn & pfn_top_mask) >> pfn_pdx_hole_shift);
}

static void pfn_pdx_hole_setup(unsigned long mask)
{
    unsigned int i, j, bottom_shift = 0, hole_shift = 0;

    /*
     * We skip the first MAX_ORDER bits, as we never want to compress them.
     * This guarantees that page-pointer arithmetic remains valid within
     * contiguous aligned ranges of 2^MAX_ORDER pages. Among others, our
     * buddy allocator relies on this assumption.
     */
    for ( j = MAX_ORDER-1; ; )
    {
        i = find_next_zero_bit(&mask, BITS_PER_LONG, j);
        j = find_next_bit(&mask, BITS_PER_LONG, i);
        if ( j >= BITS_PER_LONG )
            break;
        if ( j - i > hole_shift )
        {
            hole_shift = j - i;
            bottom_shift = i;
        }
    }
    if ( !hole_shift )
        return;

    printf("DEBUG PFN compression on bits %u...%u\n",
           bottom_shift, bottom_shift + hole_shift - 1);

    pfn_pdx_hole_shift  = hole_shift;
    pfn_pdx_bottom_mask = (1UL << bottom_shift) - 1;
    ma_va_bottom_mask   = (PAGE_SIZE << bottom_shift) - 1;
    pfn_hole_mask       = ((1UL << hole_shift) - 1) << bottom_shift;
    pfn_top_mask        = ~(pfn_pdx_bottom_mask | pfn_hole_mask);
    ma_top_mask         = pfn_top_mask << PAGE_SHIFT;
}

static void init_pdx(void)
{
    paddr_t bank_start, bank_size, bank_end;
    u64 mask;
    int bank;

    /*
     * We always map the first 4GB of RAM, hence, they are left
     * uncompressed. Thus, mask is initialized to 4GB if start of memory
     * is lower than that.
     */
    bank_start = bootinfo.mem.bank[0].start;
    if ( bank_start < 1UL << (MAX_ORDER + PAGE_SHIFT) )
        bank_start = 1UL << (MAX_ORDER + PAGE_SHIFT);
    printf("DEBUG %s %d bank_start=%lx\n",__func__,__LINE__,bank_start);
    mask = pdx_init_mask(bank_start);

    printf("DEBUG %s %d mask=%lx\n",__func__,__LINE__,mask);

    for ( bank = 0 ; bank < bootinfo.mem.nr_banks; bank++ )
    {
        bank_start = bootinfo.mem.bank[bank].start;
        bank_size = bootinfo.mem.bank[bank].size;

        mask |= bank_start | pdx_region_mask(bank_start, bank_size);
        printf("DEBUG %s %d bank_start=%lx bank_size=%lx mask=%lx\n",__func__,__LINE__,bank_start,bank_size,mask);
    }

    for ( bank = 0 ; bank < bootinfo.mem.nr_banks; bank++ )
    {
        bank_start = bootinfo.mem.bank[bank].start;
        bank_size = bootinfo.mem.bank[bank].size;


        if (~mask & pdx_region_mask(bank_start, bank_size))
        {
            mask = 0;
            printf("DEBUG %s %d mask=%lx\n",__func__,__LINE__,mask);
        }
    }

    printf("DEBUG %s %d mask=%lx\n",__func__,__LINE__,mask);
    pfn_pdx_hole_setup(mask >> PAGE_SHIFT);

    /*
    for ( bank = 0 ; bank < bootinfo.mem.nr_banks; bank++ )
    {
        bank_start = bootinfo.mem.bank[bank].start;
        bank_size = bootinfo.mem.bank[bank].size;
        bank_end = bank_start + bank_size;

        set_pdx_range(paddr_to_pfn(bank_start),
                      paddr_to_pfn(bank_end));
    }
    */
}

void setup_frametable_mappings(paddr_t ps, paddr_t pe)
{
    unsigned long nr_pdxs = pfn_to_pdx((pe >> PAGE_SHIFT) - 1) -
                            pfn_to_pdx(ps >> PAGE_SHIFT) + 1;
    unsigned long frametable_size = nr_pdxs * sizeof(struct page_info);
    const unsigned long mapping_size = frametable_size < MB(32) ? MB(2) : MB(32);
    unsigned long nr_second;
    int i;

    printf("DEBUG %s %d start=%lx end=%lx nr_ldxs=%lu\n",__func__,__LINE__,ps,pe,nr_pdxs);
    printf("DEBUG pfn_pdx_bottom_mask=%lx pfn_top_mask=%lx pfn_pdx_hole_shift=%x\n",pfn_pdx_bottom_mask,pfn_top_mask,pfn_pdx_hole_shift);
    printf("DEBUG size_pdx_memory=%lu tot_size=%lu diff(MB)=%lu\n",nr_pdxs*PAGE_SIZE,tot_size,((nr_pdxs*PAGE_SIZE)-tot_size)/(1024*1024));
}

int main()
{
    printf("DEBUG TEST ZYNQMP\n");
    bootinfo.mem.bank[0].start = 0x0;
    bootinfo.mem.bank[0].size = 0x7ff00000;
    bootinfo.mem.bank[1].start = (unsigned long long)0x800000000;
    bootinfo.mem.bank[1].size = 0x80000000;
    bootinfo.mem.nr_banks = 2;
    tot_size = bootinfo.mem.bank[0].size + bootinfo.mem.bank[1].size;

    init_pdx();
    setup_frametable_mappings(0x0, 0x880000000);
    printf("=====\n");


    printf("DEBUG TEST 512MB@2G\n");
    bootinfo.mem.bank[0].start = 0x80000000;
    bootinfo.mem.bank[0].size = 0x2000000;
    bootinfo.mem.nr_banks = 1;
    tot_size = bootinfo.mem.bank[0].size;

    init_pdx();
    setup_frametable_mappings(0x80000000, 0x82000000);
    printf("=====\n");
}
