#include <gtest/gtest.h>

extern "C" {
    #include <astatine.h>
}

TEST(LibAstatine, test_testing)
{
    EXPECT_EQ(0, 0);
}

TEST(LibAstatine, context_init)
{
    auto context = astatine_initialize("unit test");
    EXPECT_NE(context, nullptr);

    astatine_deinitialize(context);
}
