#include <astatine.h>
#include <gtest/gtest.h>

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

TEST(LibAstatine, compiler_passes)
{
    auto context = astatine_initialize("unit test");
    EXPECT_NE(context, nullptr);
    EXPECT_EQ(astatine_get_num_passes(context), 0);

    astatine_register_pass(context, astatine_lexer_pass);
    EXPECT_EQ(astatine_get_num_passes(context), 1);

    AstatinePassFn_T buffer[1] = {};
    astatine_get_passes(context, buffer);
    EXPECT_EQ(buffer[0], astatine_lexer_pass);

    astatine_deinitialize(context);
}